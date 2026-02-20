#' Write a self-contained Shinylive-compatible app directory
#'
#' Takes a Shiny app object (e.g., from [app_deliverable()]) and writes it
#' to a directory as a self-contained app that can run in Shinylive/webR
#' **without** needing the `work` package at runtime.
#'
#' The function extracts the fully-constructed UI (HTML tags) and server
#' function (with all data captured in its closure) from the app object,
#' saves them as RDS files, and writes a minimal `app.R` that only needs
#' `shiny` to reconstruct and serve the app.
#'
#' The output directory can then be passed to [deploy_build_bundle()] to
#' create a distributable bundle.
#'
#' @param app A Shiny app object, typically from [app_deliverable()].
#' @param app_dir Character. Path to the output directory. Created if it
#'   doesn't exist. Any existing `app.R`, `ui.rds`, and `server.rds` files
#'   in the directory will be overwritten.
#'
#' @return The `app_dir` path, invisibly. This can be piped directly to
#'   [deploy_build_bundle()].
#'
#' @examples
#' \dontrun{
#' mod <- app_deliverable_add_turf(
#'   best_combo_results = turf_results,
#'   raw       = raw_data,
#'   vars      = dictionary$variable,
#'   subgroups = c("Total", "Gen_Z"),
#'   weight    = "weight",
#'   labels    = dictionary,
#'   project_name = "Ice Cream Study"
#' )
#'
#' app_deliverable(
#'   title   = "Ice Cream Study",
#'   modules = list(mod)
#' ) %>%
#'   deploy_write_app("~/Desktop/ice-cream-app") %>%
#'   deploy_build_bundle(app_name = "Ice Cream Study", extension = "kadra")
#' }
#'
#' @export
deploy_write_app <- function(app, app_dir) {
  if (!inherits(app, "shiny.appobj")) {
    cli::cli_abort(c(
      "{.arg app} must be a Shiny app object (e.g., from {.fn app_deliverable}).",
      "x" = "Got: {.cls {class(app)}}"
    ))
  }
  rlang::check_required(app_dir)

  fs::dir_create(app_dir, recurse = TRUE)

  # Extract the fully-constructed UI and server from the app object.
  # UI is stored inside the httpHandler's closure; server via serverFuncSource().
  app_ui <- get("ui", envir = environment(app$httpHandler))
  app_server <- app$serverFuncSource()

  # Save UI and server as separate RDS files.
  # The UI is pure HTML tags (data) and the server closure carries all
  # captured data (data frames, pre-computed results, etc.) in its environment.
  saveRDS(app_ui, fs::path(app_dir, "ui.rds"))
  saveRDS(app_server, fs::path(app_dir, "server.rds"))

  # Write a minimal app.R that only needs shiny + htmltools.
  # shinylive/renv will detect these library() calls and include them.
  writeLines(
    c(
      "library(shiny)",
      "library(htmltools)",
      "library(bslib)",
      "",
      "ui <- readRDS(\"ui.rds\")",
      "server <- readRDS(\"server.rds\")",
      "shinyApp(ui = ui, server = server)"
    ),
    fs::path(app_dir, "app.R")
  )

  ui_size <- file.size(fs::path(app_dir, "ui.rds"))
  server_size <- file.size(fs::path(app_dir, "server.rds"))
  total <- .deploy_format_size(ui_size + server_size)
  cli::cli_alert_success("App written to {.path {app_dir}} ({total})")

  invisible(app_dir)
}


#' Create a distributable Shinylive app bundle
#'
#' Takes a Shiny application (an `app.R` file or a directory containing one),
#' exports it via [shinylive::export()], and packages the result into a
#' compressed `.{extension}` bundle file that can be loaded by a launcher built
#' with \code{\link{deploy_launcher}}.
#'
#' Two bundle types are supported:
#' \describe{
#'   \item{Lightweight bundle (default)}{Contains only `app.json` and any
#'     extra R packages not part of base webR. Requires a launcher with a
#'     bundled shinylive runtime to run. Typically ~100–200 KB.}
#'   \item{Full bundle}{Contains the complete shinylive export (runtime, webR,
#'     base packages, and app). Can run standalone without a launcher runtime.
#'     Typically ~40–50 MB.}
#' }
#'
#' @param app_dir Character. Path to a directory containing an `app.R` file,
#'   or a direct path to an `app.R` file. If a file path is given, its parent
#'   directory is used as the app directory.
#' @param output_dir Character. Directory where the bundle file(s) will be
#'   placed. Created if it doesn't exist. Defaults to the current working
#'   directory.
#' @param app_name Character or `NULL`. A human-readable display name for the
#'   app. Used to derive the bundle filename (slugified). If `NULL`, the name
#'   is derived from the app directory name.
#' @param icon Character or `NULL`. Path to a local `.png` or `.jpg` image
#'   file, or a URL to one. Included as the app's icon inside the bundle.
#'   URLs are automatically downloaded. When the bundle is imported into a
#'   launcher, this icon is displayed on the app card instead of a generic
#'   initial letter. The image should be square, ideally 256x256 pixels
#'   (512x512 max); larger images are displayed scaled down. Keep file size
#'   under 100 KB to avoid bloating the lightweight bundle.
#' @param icon_background Character. CSS color value for the background behind
#'   the icon image on the app card. Stored in the bundle metadata and applied
#'   by the launcher when displaying the card. Defaults to `"#ffffff"` (white).
#' @param icon_border Character. CSS color value for the border around the
#'   icon on the app card. Defaults to `"#000000"` (black).
#' @param extension Character. The file extension (without dot) for the output
#'   bundle. This should match the `file_extension` used when building the
#'   target launcher with \code{\link{deploy_launcher}}. Defaults to `"resondex"`.
#' @param default_packages Character vector or `NULL`. Names of R packages that
#'   are already pre-loaded in the target launcher's runtime (via the
#'   `default_packages` or `include_recommended_packages` arguments of
#'   \code{\link{deploy_launcher}}). Packages in this list (and their
#'   dependencies) are excluded from the bundle, dramatically reducing its
#'   size. Defaults to \code{\link{deploy_recommended_packages}()}, which
#'   matches the launcher default of `include_recommended_packages = TRUE`.
#'   Set to `NULL` to include all packages in the bundle.
#' @param full_bundle Logical. If `TRUE`, also creates a full/standalone bundle
#'   containing the complete shinylive export alongside the lightweight bundle.
#'   The full bundle is named `{slug}-full.{extension}`. Defaults to `FALSE`.
#'
#' @return A character vector of paths to the created bundle file(s), returned
#'   invisibly. The first element is always the lightweight bundle. If
#'   `full_bundle = TRUE`, the second element is the full bundle path.
#'
#' @details
#' ## Build Process
#'
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates that the [shinylive][shinylive::export] package is
#'     installed and that the app directory contains an `app.R` file
#'   \item Calls [shinylive::export()] to create a complete static site in a
#'     temporary directory
#'   \item Extracts `app.json` and any extra R packages from the export into
#'     a lightweight bundle zip
#'   \item If `icon` is provided, copies the icon into the bundle and writes
#'     a `bundle-meta.json` metadata file
#'   \item If `full_bundle = TRUE`, also zips the entire export directory into
#'     a standalone bundle
#'   \item Copies the resulting bundle file(s) to `output_dir`
#' }
#'
#' ## Lightweight vs Full Bundles
#'
#' \strong{Lightweight bundles} are designed to work with a launcher built by
#' \code{\link{deploy_launcher}}. The launcher already includes the shinylive runtime
#' (~59 MB), so the lightweight bundle only needs the app-specific files.
#' This dramatically reduces bundle size — a typical app with one or two extra
#' packages produces a bundle under 200 KB.
#'
#' \strong{Full bundles} include everything needed to run the app, including the
#' shinylive framework, webR engine, and base R packages. They are larger
#' (~40–50 MB) but can be used independently or with launchers that detect and
#' handle full bundles.
#'
#' ## Bundle Contents
#'
#' A lightweight bundle zip contains:
#' \itemize{
#'   \item `app.json` — The app's R source files serialized as JSON
#'   \item `packages/` — Any additional R packages (compiled to WebAssembly)
#'     that are not included in the base webR distribution. Only present if the
#'     app uses non-base packages.
#'   \item `bundle-meta.json` — Optional metadata (present when `icon` is
#'     provided). Contains the app name and icon filename.
#'   \item `icon.png` — Optional app icon image (present when `icon` is
#'     provided).
#' }
#'
#' ## App Icon
#'
#' When an `icon` is provided, it is included in the bundle as `icon.png`
#' along with a `bundle-meta.json` file. Launchers that support per-app icons
#' read this metadata when importing the bundle and display the icon on the
#' app's card in the launcher UI.
#'
#' @examples
#' \dontrun{
#' # Basic usage: create a lightweight bundle from an app directory
#' deploy_build_bundle(
#'   app_dir = "path/to/my-app",
#'   output_dir = "~/Desktop/bundles"
#' )
#'
#' # Specify a custom app name and file extension
#' deploy_build_bundle(
#'   app_dir = "path/to/my-app/app.R",
#'   app_name = "My Data Dashboard",
#'   extension = "acmeapp",
#'   output_dir = "dist"
#' )
#'
#' # Include an app icon for the launcher card
#' deploy_build_bundle(
#'   app_dir = "path/to/my-app",
#'   app_name = "Sales Report",
#'   icon = "assets/sales-icon.png",
#'   output_dir = "dist"
#' )
#'
#' # Create both lightweight and full bundles
#' deploy_build_bundle(
#'   app_dir = "path/to/my-app",
#'   app_name = "Iris Predictor",
#'   icon = "assets/iris-icon.png",
#'   full_bundle = TRUE,
#'   output_dir = "~/Desktop/bundles"
#' )
#' }
#'
#' @export
deploy_build_bundle <- function(
    app_dir,
    output_dir = ".",
    app_name = NULL,
    icon = NULL,
    icon_background = "#ffffff",
    icon_border = "#000000",
    extension = "resondex",
    default_packages = deploy_recommended_packages(),
    full_bundle = FALSE
) {
  # ---- Check shinylive ----
  if (!requireNamespace("shinylive", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg shinylive} package is required for {.fn deploy_build_bundle}.",
      "i" = "Install it with: {.code install.packages('shinylive')}"
    ))
  }

  # ---- Resolve app_dir ----
  app_dir <- fs::path_expand(app_dir)

  # If user pointed at an app.R file directly, use its parent directory
  if (fs::is_file(app_dir)) {
    if (tolower(fs::path_file(app_dir)) != "app.r") {
      cli::cli_abort(c(
        "Expected a directory or an {.file app.R} file.",
        "x" = "Got: {.path {app_dir}}"
      ))
    }
    app_dir <- fs::path_dir(app_dir)
  }

  if (!fs::dir_exists(app_dir)) {
    cli::cli_abort("App directory not found: {.path {app_dir}}")
  }

  app_file <- fs::path(app_dir, "app.R")
  if (!fs::file_exists(app_file)) {
    cli::cli_abort(c(
      "No {.file app.R} found in {.path {app_dir}}.",
      "i" = "The app directory must contain an {.file app.R} file."
    ))
  }

  # ---- Validate inputs ----
  stopifnot(
    is.character(extension), nchar(extension) > 0,
    is.logical(full_bundle)
  )
  if (!is.null(default_packages)) {
    stopifnot(is.character(default_packages), length(default_packages) > 0)
  }
  output_dir <- fs::path_abs(fs::path_expand(output_dir))

  # ---- Derive app_name ----
  if (is.null(app_name)) {
    app_name <- fs::path_file(app_dir)
    cli::cli_alert_info("Using directory name as app name: {.val {app_name}}")
  }

  # Slugify the name for the filename
  slug <- tolower(app_name)
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("^-|-$", "", slug)

  # ---- Resolve icon (local path or URL) ----
  if (!is.null(icon)) {
    icon <- .deploy_resolve_file_or_url(icon, label = "icon",
                                       valid_extensions = c("png", "jpg", "jpeg"))
  }

  # ---- Step 1: Shinylive export ----
  # Copy app files to a clean temp directory, excluding build artifacts
  # that would confuse shinylive (e.g. a previous site/ export containing
  # shinylive.js triggers a massive re-scan and can crash R)
  clean_app_dir <- tempfile(pattern = "clean-app-")
  fs::dir_create(clean_app_dir)
  on.exit(fs::dir_delete(clean_app_dir), add = TRUE)

  skip_dirs <- c("site", "dist", "node_modules", ".git", "build-icons",
                   "rsconnect", "packrat", "renv")
  # Also skip directories that look like Shiny diskCache dirs
  # (contain mostly .rds files with hex hash names)
  for (entry in fs::dir_ls(app_dir, type = "directory")) {
    entry_name <- fs::path_file(entry)
    if (entry_name %in% skip_dirs) next
    rds_files <- list.files(entry, pattern = "\\.rds$", recursive = FALSE)
    if (length(rds_files) > 5) {
      hex_names <- grepl("^[0-9a-f]+\\.rds$", rds_files)
      if (sum(hex_names) / length(rds_files) > 0.8) {
        skip_dirs <- c(skip_dirs, entry_name)
        cli::cli_alert_info(
          "Skipping cache directory: {.path {entry_name}} ({length(rds_files)} cached files)"
        )
      }
    }
  }
  app_entries <- fs::dir_ls(app_dir, all = FALSE)
  for (entry in app_entries) {
    entry_name <- fs::path_file(entry)
    if (entry_name %in% skip_dirs) next
    # Skip bundle files (*.resondex, *.kadro, etc.)
    if (grepl(paste0("\\.", extension, "$"), entry_name)) next
    dest <- fs::path(clean_app_dir, entry_name)
    if (fs::is_dir(entry)) {
      fs::dir_copy(entry, dest)
    } else {
      fs::file_copy(entry, dest)
    }
  }

  # ---- Patch for Shinylive compatibility ----
  .deploy_patch_shinylive_compat(clean_app_dir)

  export_dir <- tempfile(pattern = "shinylive-export-")
  fs::dir_create(export_dir)
  on.exit(fs::dir_delete(export_dir), add = TRUE)

  # Skip downloading wasm packages when default_packages covers the app's deps —

  # the launcher runtime already has them. This makes export much faster.
  skip_wasm <- !is.null(default_packages) && length(default_packages) > 0
  if (skip_wasm) {
    cli::cli_alert_info("Running shinylive::export() (skipping wasm packages — launcher has them)...")
  } else {
    cli::cli_alert_info("Running shinylive::export() (downloading assets & packages)...")
  }
  shinylive::export(clean_app_dir, export_dir, quiet = TRUE,
                    wasm_packages = !skip_wasm)
  cli::cli_alert_success("Shinylive export complete")

  # ---- Step 2: Create lightweight bundle ----
  cli::cli_alert_info("Creating lightweight bundle...")

  bundle_tmp <- tempfile(pattern = "bundle-tmp-")
  fs::dir_create(bundle_tmp)
  on.exit(fs::dir_delete(bundle_tmp), add = TRUE)

  # Copy app.json
  app_json <- fs::path(export_dir, "app.json")
  if (!fs::file_exists(app_json)) {
    cli::cli_abort("shinylive::export() did not produce an {.file app.json}. Something went wrong.")
  }
  fs::file_copy(app_json, fs::path(bundle_tmp, "app.json"))

  # Copy extra R packages if present, filtering out default packages
  pkg_dir <- fs::path(export_dir, "shinylive", "webr", "packages")
  if (fs::dir_exists(pkg_dir)) {
    pkg_entries <- fs::dir_ls(pkg_dir)
    if (length(pkg_entries) > 0) {
      dest_pkg <- fs::path(bundle_tmp, "packages")
      fs::dir_create(dest_pkg)

      # Determine which package subdirectories to skip (already in launcher runtime)
      # Packages are stored as subdirectories: packages/{name}/{name}_{version}.tgz
      skip_pkg_dirs <- character()
      if (!is.null(default_packages)) {
        cli::cli_alert_info("Resolving default packages to filter from bundle...")
        resolved <- .deploy_resolve_default_packages(default_packages)
        # Only clean up temp export dir; cached results are persistent
        if (!resolved$cached) {
          on.exit(fs::dir_delete(resolved$export_dir), add = TRUE)
        }
        # Get the subdirectory names (package names) from the default packages export
        default_dirs <- fs::dir_ls(resolved$packages_dir, type = "directory")
        skip_pkg_dirs <- fs::path_file(default_dirs)
      }

      included <- 0L
      skipped <- 0L
      skipped_size <- 0
      for (entry in pkg_entries) {
        entry_name <- fs::path_file(entry)

        # Always keep metadata.rds — bundle needs the complete manifest
        if (entry_name == "metadata.rds") {
          fs::file_copy(entry, fs::path(dest_pkg, entry_name))
          included <- included + 1L
          next
        }

        # Skip package subdirectories that match default packages
        if (entry_name %in% skip_pkg_dirs) {
          skipped <- skipped + 1L
          # Sum the size of all .tgz files in the skipped subdirectory
          tgz_in_dir <- list.files(as.character(entry), pattern = "\\.tgz$",
                                   full.names = TRUE, recursive = TRUE)
          skipped_size <- skipped_size + sum(file.size(tgz_in_dir))
          next
        }

        if (fs::is_dir(entry)) {
          fs::dir_copy(entry, fs::path(dest_pkg, entry_name))
        } else {
          fs::file_copy(entry, fs::path(dest_pkg, entry_name))
        }
        included <- included + 1L
      }

      if (skipped > 0L) {
        cli::cli_alert_success(
          "Skipped {skipped} package(s) already in launcher ({(.deploy_format_size(skipped_size))}), included {included} file(s)"
        )
      } else {
        cli::cli_alert_info("Including {included} extra R package file(s)...")
      }
    }
  }

  # Copy icon and write bundle-meta.json
  meta <- list(
    appName = app_name,
    iconBackground = icon_background,
    iconBorder = icon_border
  )

  if (!is.null(icon)) {
    cli::cli_alert_info("Including app icon...")
    fs::file_copy(icon, fs::path(bundle_tmp, "icon.png"), overwrite = TRUE)
    meta$icon <- "icon.png"
  }

  jsonlite::write_json(meta, fs::path(bundle_tmp, "bundle-meta.json"),
                       auto_unbox = TRUE, pretty = TRUE)
  cli::cli_alert_success("Bundle metadata written")

  # Zip into bundle
  fs::dir_create(output_dir)
  bundle_filename <- paste0(slug, ".", extension)
  bundle_path <- fs::path(output_dir, bundle_filename)

  # zip::zip wants relative paths, so use withr::with_dir
  files_to_zip <- fs::path_file(fs::dir_ls(bundle_tmp, all = TRUE, recurse = TRUE,
                                            type = "file"))
  # Use relative paths from bundle_tmp
  withr::with_dir(bundle_tmp, {
    all_files <- list.files(".", recursive = TRUE, all.files = TRUE)
    zip::zip(
      zipfile = as.character(bundle_path),
      files = all_files,
      mode = "cherry-pick"
    )
  })

  size_bytes <- file.size(as.character(bundle_path))
  size_display <- .deploy_format_size(size_bytes)
  cli::cli_alert_success("Created: {.path {bundle_filename}} ({size_display})")

  result_paths <- as.character(bundle_path)

  # ---- Step 3: Create full bundle (optional) ----
  if (full_bundle) {
    cli::cli_alert_info("Creating full bundle...")
    full_filename <- paste0(slug, "-full.", extension)
    full_path <- fs::path(output_dir, full_filename)

    # Add icon and meta to the full export as well
    if (!is.null(icon)) {
      fs::file_copy(icon, fs::path(export_dir, "icon.png"), overwrite = TRUE)
      meta <- list(appName = app_name, icon = "icon.png")
      jsonlite::write_json(meta, fs::path(export_dir, "bundle-meta.json"),
                           auto_unbox = TRUE, pretty = TRUE)
    }

    withr::with_dir(export_dir, {
      all_files <- list.files(".", recursive = TRUE, all.files = TRUE)
      zip::zip(
        zipfile = as.character(full_path),
        files = all_files,
        mode = "cherry-pick"
      )
    })

    full_size <- file.size(as.character(full_path))
    full_display <- .deploy_format_size(full_size)
    cli::cli_alert_success("Created: {.path {full_filename}} ({full_display})")

    result_paths <- c(result_paths, as.character(full_path))
  }

  # ---- Done ----
  cli::cli_alert_success(
    "Done! {length(result_paths)} bundle(s) created in {.path {output_dir}}"
  )

  invisible(result_paths)
}


#' Format file size for display
#'
#' Converts a file size in bytes to a human-readable string.
#'
#' @param bytes Numeric. File size in bytes.
#' @return Character string like `"140 KB"` or `"44.2 MB"`.
#' @noRd
.deploy_format_size <- function(bytes) {
  if (is.na(bytes) || bytes < 0) return("unknown")
  if (bytes < 1024) return(paste(bytes, "B"))
  if (bytes < 1024^2) return(paste(round(bytes / 1024, 1), "KB"))
  if (bytes < 1024^3) return(paste(round(bytes / 1024^2, 1), "MB"))
  paste(round(bytes / 1024^3, 1), "GB")
}
