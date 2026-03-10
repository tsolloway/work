#' Write a Shiny app to a directory for bundling
#'
#' Takes a Shiny app object (e.g., from [app_deliverable()]) and writes it
#' to a directory that can be run with `shiny::runApp()`. The app object is
#' saved as an RDS file and a minimal `app.R` is written that loads and runs
#' it.
#'
#' The output directory can then be passed to [deploy_build_bundle()] to
#' create a distributable bundle.
#'
#' @param app A Shiny app object, typically from [app_deliverable()].
#' @param app_dir Character. Path to the output directory. Created if it
#'   doesn't exist. Any existing `app.R` and `app_object.rds` files in the
#'   directory will be overwritten.
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

  # Save the entire app object as RDS
  rds_path <- fs::path(app_dir, "app_object.rds")
  saveRDS(app, rds_path, compress = "gzip")

  # Write a minimal app.R that loads the app object.

  # The last expression must evaluate to the app object — shiny::runApp()

  # will source this file and use the return value. Do NOT call
  # shiny::runApp() here, or it will start a second server on a random port.
  writeLines(
    c(
      'app <- readRDS("app_object.rds")',
      "app"
    ),
    fs::path(app_dir, "app.R")
  )

  rds_size <- file.size(as.character(rds_path))
  cli::cli_alert_success(
    "App written to {.path {app_dir}} ({(.deploy_format_size(rds_size))})"
  )

  invisible(app_dir)
}


#' Create a distributable app bundle
#'
#' Takes a directory containing an `app.R` file and packages it into a
#' compressed `.{extension}` bundle file that can be loaded by a launcher
#' built with \code{\link{deploy_launcher}}.
#'
#' The bundle contains the app files and optionally any extra R packages
#' that are not already included in the launcher's library.
#'
#' @param app_dir Character. Path to a directory containing an `app.R` file,
#'   or a direct path to an `app.R` file. If a file path is given, its parent
#'   directory is used as the app directory.
#' @param output_dir Character. Directory where the bundle file will be
#'   placed. Created if it doesn't exist. Defaults to the current working
#'   directory.
#' @param app_name Character or `NULL`. A human-readable display name for the
#'   app. Used to derive the bundle filename (slugified). If `NULL`, the name
#'   is derived from the app directory name.
#' @param icon Character or `NULL`. Path to a local `.png` or `.jpg` image
#'   file, or a URL to one. Included as the app's icon inside the bundle.
#' @param icon_background Character. CSS color for the icon background on the
#'   app card. Defaults to `"#ffffff"`.
#' @param icon_border Character. CSS color for the icon border on the app
#'   card. Defaults to `"#000000"`.
#' @param extension Character. File extension (without dot) for the output
#'   bundle. Should match the `file_extension` used when building the target
#'   launcher. Defaults to `"resondex"`.
#' @param extra_packages Character vector or `NULL`. Names of additional R
#'   packages that the app needs but are NOT already in the launcher's
#'   library. These packages will be copied from the local R library into
#'   the bundle. Defaults to `NULL` (no extra packages — assumes the
#'   launcher's library has everything needed).
#'
#' @return The path to the created bundle file, returned invisibly.
#'
#' @details
#' ## Bundle Structure
#'
#' The bundle is a zip file with the following contents:
#' \describe{
#'   \item{`app.R`}{The main app entry point}
#'   \item{`app_object.rds`}{The serialized Shiny app (if created by
#'     [deploy_write_app()])}
#'   \item{`bundle-meta.json`}{Metadata (app name, icon info)}
#'   \item{`icon.png`}{Optional app icon}
#'   \item{`library/`}{Optional directory containing extra R packages
#'     not in the launcher}
#' }
#'
#' ## Launcher vs Bundle Packages
#'
#' The launcher (built by [deploy_launcher()]) ships with base R and all
#' `work` package dependencies pre-installed. Most apps will not need any
#' extra packages. If an app uses packages outside of `work`'s dependency
#' tree, pass them via `extra_packages` and they will be included in the
#' bundle.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' deploy_build_bundle(
#'   app_dir = "path/to/my-app",
#'   app_name = "My Dashboard",
#'   extension = "kadra"
#' )
#'
#' # With extra packages not in the launcher
#' deploy_build_bundle(
#'   app_dir = "path/to/my-app",
#'   app_name = "My Dashboard",
#'   extension = "kadra",
#'   extra_packages = c("leaflet", "sf")
#' )
#'
#' # Piped from deploy_write_app()
#' app_deliverable(title = "My App", modules = list(mod)) %>%
#'   deploy_write_app("my-app") %>%
#'   deploy_build_bundle(app_name = "My App", extension = "kadra")
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
    extra_packages = NULL
) {
  # ---- Resolve app_dir ----
  app_dir <- fs::path_expand(app_dir)

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
    is.character(extension), nchar(extension) > 0
  )
  output_dir <- fs::path_abs(fs::path_expand(output_dir))

  # ---- Derive app_name ----
  if (is.null(app_name)) {
    app_name <- fs::path_file(app_dir)
    cli::cli_alert_info("Using directory name as app name: {.val {app_name}}")
  }

  slug <- tolower(app_name)
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("^-|-$", "", slug)

  # ---- Resolve icon ----
  if (!is.null(icon)) {
    icon <- .deploy_resolve_file_or_url(icon, label = "icon",
                                        valid_extensions = c("png", "jpg", "jpeg"))
  }

  # ---- Create bundle staging directory ----
  cli::cli_alert_info("Creating bundle...")

  bundle_tmp <- tempfile(pattern = "bundle-tmp-")
  fs::dir_create(bundle_tmp)
  on.exit(fs::dir_delete(bundle_tmp), add = TRUE)

  # Copy all app files to the staging directory
  app_entries <- fs::dir_ls(app_dir, all = FALSE)
  skip_patterns <- c("site", "dist", "node_modules", ".git", "rsconnect",
                     "packrat", "renv", "state.rds")
  for (entry in app_entries) {
    entry_name <- fs::path_file(entry)
    if (entry_name %in% skip_patterns) next
    # Skip existing bundle files
    if (grepl(paste0("\\.", extension, "$"), entry_name)) next
    dest <- fs::path(bundle_tmp, entry_name)
    if (fs::is_dir(entry)) {
      fs::dir_copy(entry, dest)
    } else {
      fs::file_copy(entry, dest)
    }
  }

  # ---- Copy extra packages ----
  if (!is.null(extra_packages) && length(extra_packages) > 0) {
    cli::cli_alert_info("Including {length(extra_packages)} extra package(s)...")

    # Determine which are already in the launcher
    launcher_pkgs <- deploy_recommended_packages()
    extras_needed <- setdiff(extra_packages, launcher_pkgs)

    if (length(extras_needed) == 0) {
      cli::cli_alert_success("All extra packages already in launcher — none to add")
    } else {
      lib_dest <- fs::path(bundle_tmp, "library")
      fs::dir_create(lib_dest)

      for (pkg in extras_needed) {
        pkg_path <- find.package(pkg, quiet = TRUE)
        if (length(pkg_path) == 0) {
          cli::cli_alert_warning("Package {.pkg {pkg}} not found in local library, skipping")
          next
        }
        dest_pkg <- fs::path(lib_dest, pkg)
        fs::dir_copy(pkg_path, dest_pkg)
        # Strip fat from the copied package
        .deploy_strip_package_fat(dest_pkg)
        cli::cli_alert_success("Included {.pkg {pkg}}")
      }
    }
  }

  # ---- Bundle metadata ----
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

  # ---- Zip into bundle ----
  fs::dir_create(output_dir)
  bundle_filename <- paste0(slug, ".", extension)
  bundle_path <- fs::path(output_dir, bundle_filename)

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

  invisible(as.character(bundle_path))
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
