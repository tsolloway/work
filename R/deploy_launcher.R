#' Deploy a branded Shiny Launcher desktop application
#'
#' Creates a branded Electron-based desktop application with cross-platform
#' installers. The launcher serves as a hub where end users (who don't have R)
#' can load and run Shinylive app bundles.
#'
#' @param app_name Character. The display name of the launcher application.
#'   This appears in the window title, macOS menu bar, installer name, and
#'   OS file association descriptions.
#' @param version Character. Semantic version string for the launcher
#'   (e.g., `"1.0.0"`, `"2.1.3"`). Shown in the About dialog and baked into
#'   the installer metadata. Defaults to `"1.0.0"`.
#' @param header_title Character. The large heading text displayed in the
#'   launcher's header bar. Defaults to `app_name`.
#' @param header_subtitle Character. The subtitle text displayed below the
#'   header title. Defaults to `"Load and run Shinylive apps"`.
#' @param header_background Character. The header background, specified as
#'   either a CSS color value (e.g., `"#1a1a2e"`, `"rgb(26,26,46)"`), a
#'   path to a local image file (e.g., `"assets/header-bg.png"`), or a URL
#'   to an image (e.g., `"https://example.com/bg.jpg"`). URLs are
#'   automatically downloaded. For images, recommended dimensions are at
#'   least 1200x200 pixels (or wider) in `.png` or `.jpg` format; the image
#'   is displayed as `center/cover` so wider landscape images work best.
#'   Defaults to `"#1a1a2e"`.
#' @param accent_color Character. Hex color code for accent and interactive
#'   elements including buttons, card hover borders, the floating action
#'   button, loading indicators, and input focus borders. Defaults to
#'   `"#4361ee"`.
#' @param icon Character or `NULL`. Path to a local application icon file or
#'   a URL to one. Accepts `.png`, `.jpg` (works on all platforms), `.icns`
#'   (macOS), or `.ico` (Windows). URLs are automatically downloaded. The
#'   image should be square and at least 512x512 pixels (1024x1024
#'   recommended); electron-builder uses this to generate all required icon
#'   sizes. If `NULL` (the default), the bundled Resondex logo is used. The
#'   icon appears in the window title bar, taskbar/dock, and installer.
#' @param file_extension Character. The custom file extension (without dot)
#'   that the launcher accepts and associates with at the OS level. End users
#'   can double-click files with this extension to open them in the launcher.
#'   Defaults to `"resondex"`.
#' @param output_dir Character. Directory where the installer files will be
#'   placed. Created if it doesn't exist. Defaults to the current working
#'   directory.
#' @param targets Character vector. Which platform installers to build. One
#'   or more of:
#'   \itemize{
#'     \item `"mac-arm64"` — macOS Apple Silicon DMG
#'     \item `"mac-x64"` — macOS Intel DMG
#'     \item `"win-x64"` — Windows x64 NSIS installer (.exe)
#'   }
#'   Defaults to all three.
#' @param default_packages Character vector or `NULL`. Names of R packages to
#'   pre-load into the launcher runtime. These packages (and all their
#'   dependencies) are resolved via [shinylive::export()] and included in the
#'   runtime's `packages/` directory. Bundles built with [deploy_build_bundle()] can
#'   then pass the same list to `default_packages` to exclude these packages,
#'   producing much smaller bundles. Defaults to `NULL` (no extra packages).
#' @param include_recommended_packages Logical. If `TRUE`, merges the curated
#'   list from [deploy_recommended_packages()] into `default_packages`. This is the
#'   easiest way to include a broad set of commonly used packages. The
#'   recommended set covers tidyverse, visualization (plotly, leaflet, DT,
#'   highcharter), Shiny UI (bslib, shinyWidgets, shinyjs), and more.
#'   Defaults to `TRUE`.
#' @param cleanup Logical. Whether to remove the temporary build directory
#'   after completion. Set to `FALSE` to inspect the build output for
#'   debugging. Defaults to `TRUE`.
#'
#' @return A character vector of paths to the created installer files,
#'   returned invisibly.
#'
#' @details
#' ## Build Process
#'
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates inputs and checks that Node.js and npm are installed
#'   \item Copies the launcher template (including the shinylive/webR runtime)
#'     to a temporary build directory
#'   \item Writes `launcher.config.json` with your branding parameters
#'   \item Runs `node sync-config.js` to propagate config into `package.json`
#'   \item Runs `npm install` to install Electron and electron-builder
#'   \item Runs `electron-builder` for each target platform
#'   \item Copies the resulting installer files to `output_dir`
#' }
#'
#' ## Cross-Platform Notes
#'
#' Building macOS DMGs on macOS works natively. Building Windows NSIS
#' installers from macOS uses electron-builder's cross-compilation support,
#' which works but does not support Windows code signing.
#'
#' ## File Association
#'
#' The built launcher registers the specified `file_extension` with the
#' operating system. When installed, users can double-click `.{extension}`
#' files to open them directly in the launcher.
#'
#' @examples
#' \dontrun{
#' # Basic usage with defaults
#' deploy_launcher(
#'   app_name = "My Data Tools",
#'   output_dir = "~/Desktop/installers"
#' )
#'
#' # Fully customized branding
#' deploy_launcher(
#'   app_name = "Acme Analytics",
#'   header_title = "Acme Analytics Suite",
#'   header_subtitle = "Interactive dashboards for your team",
#'   header_background = "#2d3436",
#'   accent_color = "#e17055",
#'   icon = "assets/acme-icon.png",
#'   file_extension = "acmeapp",
#'   output_dir = "dist/installers",
#'   targets = c("mac-arm64", "win-x64")
#' )
#'
#' # Build only macOS installers, keep build dir for debugging
#' deploy_launcher(
#'   app_name = "Debug Test",
#'   targets = c("mac-arm64", "mac-x64"),
#'   cleanup = FALSE
#' )
#' }
#'
#' @export
deploy_launcher <- function(
    app_name,
    version = "1.0.0",
    header_title = app_name,
    header_subtitle = "Load and run Shinylive apps",
    header_background = "#1a1a2e",
    accent_color = "#4361ee",
    icon = NULL,
    file_extension = "resondex",
    default_packages = NULL,
    include_recommended_packages = TRUE,
    output_dir = ".",
    targets = c("mac-arm64", "mac-x64", "win-x64"),
    cleanup = TRUE
) {
  # ---- Validate inputs ----
  rlang::check_required(app_name)
  stopifnot(
    is.character(app_name), nchar(app_name) > 0,
    is.character(version), nchar(version) > 0,
    is.character(header_title), nchar(header_title) > 0,
    is.character(header_subtitle),
    is.character(header_background),
    is.character(accent_color),
    is.character(file_extension), nchar(file_extension) > 0,
    is.logical(include_recommended_packages),
    is.logical(cleanup)
  )
  if (!is.null(default_packages)) {
    stopifnot(is.character(default_packages), length(default_packages) > 0)
  }
  targets <- match.arg(targets, c("mac-arm64", "mac-x64", "win-x64"),
                       several.ok = TRUE)
  output_dir <- fs::path_expand(output_dir)

  # ---- Check prerequisites ----
  deploy_check_prerequisites()

  # ---- Ensure runtime is populated ----
  template_dir_check <- system.file("deploy_template", package = "work")
  if (template_dir_check != "") {
    runtime_dir <- fs::path(template_dir_check, "runtime")
    runtime_has_content <- fs::dir_exists(runtime_dir) &&
      length(list.files(runtime_dir, pattern = "\\.(js|html)$", recursive = TRUE)) > 0
    if (!runtime_has_content) {
      cli::cli_alert_info("Runtime not found in package — populating from shinylive cache...")
      # For installed packages, system.file() returns .../deploy_template
      # and the package root is one level up
      pkg_root <- fs::path_dir(template_dir_check)
      deploy_update_runtime(package_dir = pkg_root)
    }
  }

  # ---- Resolve icon (local path, URL, or bundled default) ----
  icon_rel <- NULL
  if (is.null(icon)) {
    # Use the bundled Resondex logo as the default icon
    icon <- system.file("deploy_template", "resondex-logo.png", package = "work")
    if (icon == "") {
      cli::cli_alert_warning("Default icon not found in package — using Electron default")
      icon <- NULL
    }
  } else {
    icon <- .deploy_resolve_file_or_url(icon, label = "icon",
                                       valid_extensions = c("png", "jpg", "jpeg", "icns", "ico"))
  }

  # ---- Resolve header_background if it's an image URL ----
  # CSS color values are passed through as-is; URLs are downloaded
  if (grepl("^https?://", header_background)) {
    header_background <- .deploy_resolve_file_or_url(
      header_background, label = "header_background"
    )
    # Will be copied into build dir below alongside the icon step
  }

  # ---- Create build directory ----
  build_dir <- tempfile(pattern = "resondex-build-")
  fs::dir_create(build_dir)
  if (cleanup) {
    on.exit(fs::dir_delete(build_dir), add = TRUE)
  } else {
    cli::cli_alert_info("Build directory (not cleaned up): {.path {build_dir}}")
  }

  # ---- Step 1: Copy template ----
  cli::cli_alert_info("Copying launcher template...")
  template_dir <- system.file("deploy_template", package = "work")
  if (template_dir == "" || !fs::dir_exists(template_dir)) {
    cli::cli_abort("Package template directory not found. Is {.pkg work} installed correctly?")
  }

  # fs::dir_copy copies src as a subdirectory of dest, so we copy contents
  template_files <- fs::dir_ls(template_dir, all = TRUE)
  for (f in template_files) {
    fname <- fs::path_file(f)
    if (fname %in% c(".", "..", ".DS_Store")) next
    dest <- fs::path(build_dir, fname)
    if (fs::is_dir(f)) {
      fs::dir_copy(f, dest)
    } else {
      fs::file_copy(f, dest)
    }
  }
  cli::cli_alert_success("Template copied ({length(template_files)} items)")

  # ---- Step 1b: Populate default packages ----
  if (include_recommended_packages) {
    default_packages <- unique(c(default_packages, deploy_recommended_packages()))
  }
  if (!is.null(default_packages)) {
    cli::cli_alert_info(
      "Resolving {length(default_packages)} default package(s)..."
    )
    resolved <- .deploy_resolve_default_packages(default_packages)
    # Only clean up temp export dir; cached results are persistent
    if (!resolved$cached) {
      on.exit(fs::dir_delete(resolved$export_dir), add = TRUE)
    }

    dest_pkg_dir <- fs::path(build_dir, "runtime", "shinylive", "webr", "packages")
    fs::dir_create(dest_pkg_dir)

    pkg_files <- fs::dir_ls(resolved$packages_dir)
    for (f in pkg_files) {
      fname <- fs::path_file(f)
      if (fs::is_dir(f)) {
        fs::dir_copy(f, fs::path(dest_pkg_dir, fname))
      } else {
        fs::file_copy(f, fs::path(dest_pkg_dir, fname), overwrite = TRUE)
      }
    }

    # Count .tgz files recursively (they live in subdirectories: {name}/{name}_{version}.tgz)
    tgz_files <- list.files(dest_pkg_dir, pattern = "\\.tgz$",
                            recursive = TRUE, full.names = TRUE)
    tgz_count <- length(tgz_files)
    total_size <- sum(file.size(tgz_files))
    cli::cli_alert_success(
      "Included {tgz_count} default package .tgz file(s) ({(.deploy_format_size(total_size))}) in runtime"
    )
  }

  # ---- Step 2: Copy icon ----
  if (!is.null(icon)) {
    cli::cli_alert_info("Copying icon file...")
    icon_filename <- paste0("icon.", fs::path_ext(icon))
    fs::file_copy(icon, fs::path(build_dir, icon_filename), overwrite = TRUE)
    icon_rel <- icon_filename
    cli::cli_alert_success("Icon: {.file {icon_filename}}")
  }

  # ---- Step 2b: Copy header background image (if it was downloaded/local) ----
  # The image must live inside renderer/ because CSS url() resolves relative
  # to renderer/index.html where the styles are loaded
  if (!is.null(header_background) && fs::file_exists(header_background)) {
    bg_filename <- paste0("header-bg.", fs::path_ext(header_background))
    renderer_dir <- fs::path(build_dir, "renderer")
    fs::dir_create(renderer_dir)
    fs::file_copy(header_background, fs::path(renderer_dir, bg_filename),
                  overwrite = TRUE)
    # Config stores just the filename — renderer resolves it relative to itself
    header_background <- bg_filename
    cli::cli_alert_success("Header background image: {.file renderer/{bg_filename}}")
  }

  # ---- Step 3: Write launcher.config.json ----
  cli::cli_alert_info("Writing launcher configuration...")
  .deploy_write_launcher_config(
    config_path = fs::path(build_dir, "launcher.config.json"),
    app_name = app_name,
    version = version,
    header_title = header_title,
    header_subtitle = header_subtitle,
    header_background = header_background,
    accent_color = accent_color,
    icon = icon_rel,
    file_extension = file_extension
  )
  cli::cli_alert_success("Config written")

  # ---- Step 4: Sync config to package.json ----
  .deploy_run_command("node", "sync-config.js", wd = build_dir,
              label = "node sync-config.js")

  # ---- Step 5: npm install ----
  cli::cli_alert_info("Installing Node.js dependencies (this may take a minute)...")
  .deploy_run_command("npm", "install", wd = build_dir, label = "npm install")

  # ---- Step 6: Build installers ----
  fs::dir_create(output_dir)
  installer_files <- character()

  for (target in targets) {
    cli::cli_alert_info("Building installer: {.val {target}}...")

    eb_args <- switch(target,
      "mac-arm64" = c("electron-builder", "--mac", "--arm64"),
      "mac-x64"   = c("electron-builder", "--mac", "--x64"),
      "win-x64"   = c("electron-builder", "--win", "--x64")
    )

    .deploy_run_command("npx", eb_args, wd = build_dir,
                label = paste("electron-builder", target))
    cli::cli_alert_success("{target} build complete")
  }

  # ---- Step 7: Collect installers ----
  cli::cli_alert_info("Collecting installer files...")
  dist_dir <- fs::path(build_dir, "dist")

  if (!fs::dir_exists(dist_dir)) {
    cli::cli_abort("Build completed but no {.path dist/} directory found.")
  }

  all_files <- fs::dir_ls(dist_dir, recurse = FALSE)
  # Match .dmg and .exe files, exclude .blockmap
  installers <- all_files[grepl("\\.(dmg|exe)$", all_files)]
  installers <- installers[!grepl("\\.blockmap$", installers)]

  for (installer in installers) {
    dest <- fs::path(output_dir, fs::path_file(installer))
    fs::file_copy(installer, dest, overwrite = TRUE)
    installer_files <- c(installer_files, as.character(dest))
    size <- file.size(as.character(installer))
    size_mb <- round(size / 1e6, 1)
    cli::cli_alert_success("Created: {.path {fs::path_file(dest)}} ({size_mb} MB)")
  }

  cli::cli_alert_success(
    "Done! {length(installer_files)} installer(s) created in {.path {output_dir}}"
  )

  invisible(installer_files)
}
