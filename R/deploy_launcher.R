#' Deploy a branded Shiny Launcher desktop application
#'
#' Creates a branded Electron-based desktop application with cross-platform
#' installers. The launcher serves as a hub where end users (who don't have R)
#' can load and run Shiny app bundles. The launcher ships with a bundled
#' portable R installation and all `work` package dependencies pre-installed.
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
#'   header title. Defaults to `"Load and run Shiny apps"`.
#' @param header_background Character. The header background, specified as
#'   either a CSS color value (e.g., `"#ffffff"`, `"rgb(255,255,255)"`), a
#'   path to a local image file (e.g., `"assets/header-bg.png"`), or a URL.
#'   Defaults to `"#ffffff"` (matches the neutral `bn_report` aesthetic).
#' @param header_text Character. Hex color code for the header title and
#'   subtitle text. Set to a light color (e.g., `"#ffffff"`) when using a
#'   dark `header_background`. Defaults to `"#333333"`.
#' @param accent_color Character. Hex color code for accent and interactive
#'   elements (FAB, primary buttons). Defaults to `"#333333"` for a neutral
#'   look that matches `bn_report`; pass a brand color (e.g., `"#4361ee"`)
#'   for a more vibrant launcher.
#' @param icon Character or `NULL`. Path to a local application icon file or
#'   a URL to one. Accepts `.png`, `.jpg`, `.icns`, or `.ico`. If `NULL`,
#'   the bundled Resondex logo is used.
#' @param file_extension Character. The custom file extension (without dot)
#'   that the launcher accepts. Defaults to `"resondex"`.
#' @param r_home Character or `NULL`. Path to the R installation to bundle.
#'   If `NULL` (default), uses `R.home()` from the running R session.
#' @param output_dir Character. Directory where the installer files will be
#'   placed. Created if it doesn't exist. Defaults to the current working
#'   directory.
#' @param targets Character vector. Which platform installers to build.
#'   One or more of `"mac-arm64"`, `"mac-x64"`, `"win-x64"`.
#'   Defaults to `c("mac-arm64", "win-x64")`.
#' @param cleanup Logical. Whether to remove the temporary build directory
#'   after completion. Set to `FALSE` to inspect the build output.
#'   Defaults to `TRUE`.
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
#'   \item Copies the launcher template to a temporary build directory
#'   \item Bundles a portable R installation into `RPortable/`
#'   \item Copies all `work` package dependencies into `RPortable/library/`
#'   \item Writes `launcher.config.json` with branding parameters
#'   \item Runs `npm install` and `electron-builder` for each target
#'   \item Copies the resulting installer files to `output_dir`
#' }
#'
#' ## What Ships in the Launcher
#'
#' The launcher includes:
#' \itemize{
#'   \item Base R installation (portable, no system install needed)
#'   \item All `work` package dependencies pre-installed in `RPortable/library/`
#'   \item The Electron app shell with branding
#' }
#'
#' At runtime, the launcher spawns R as a child process running
#' `shiny::runApp()`, and the Electron BrowserWindow loads the result.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' deploy_launcher(
#'   app_name = "My Data Tools",
#'   file_extension = "mydata",
#'   output_dir = "~/Desktop/installers"
#' )
#'
#' # Fully customized
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
#' }
#'
#' @export
deploy_launcher <- function(
    app_name,
    version = "1.0.0",
    header_title = app_name,
    header_subtitle = "Load and run Shiny apps",
    header_background = "#ffffff",
    header_text = "#333333",
    accent_color = "#333333",
    icon = NULL,
    file_extension = "resondex",
    r_home = NULL,
    output_dir = ".",
    targets = c("mac-arm64", "win-x64"),
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
    is.character(header_text), nchar(header_text) > 0,
    is.character(accent_color),
    is.character(file_extension), nchar(file_extension) > 0,
    is.logical(cleanup)
  )
  targets <- match.arg(targets, c("mac-arm64", "mac-x64", "win-x64"),
                       several.ok = TRUE)
  output_dir <- fs::path_expand(output_dir)

  # ---- Check prerequisites ----
  deploy_check_prerequisites()

  # ---- Resolve R home ----
  if (is.null(r_home)) {
    r_home <- R.home()
  }
  r_home <- fs::path_expand(r_home)
  if (!fs::dir_exists(r_home)) {
    cli::cli_abort("R installation not found: {.path {r_home}}")
  }
  cli::cli_alert_info("R installation: {.path {r_home}}")

  # ---- Resolve icon ----
  icon_rel <- NULL
  if (is.null(icon)) {
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
  if (grepl("^https?://", header_background)) {
    header_background <- .deploy_resolve_file_or_url(
      header_background, label = "header_background"
    )
  }

  # ---- Create build directory ----
  build_dir <- tempfile(pattern = "launcher-build-")
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

  # Copy template files (skip runtime/ and RPortable/ — we build RPortable fresh)
  skip_dirs <- c("runtime", "RPortable", "node_modules", "dist", ".DS_Store")
  template_files <- fs::dir_ls(template_dir, all = TRUE)
  copied <- 0L
  for (f in template_files) {
    fname <- fs::path_file(f)
    if (fname %in% c(".", "..") || fname %in% skip_dirs) next
    dest <- fs::path(build_dir, fname)
    if (fs::is_dir(f)) {
      fs::dir_copy(f, dest)
    } else {
      fs::file_copy(f, dest)
    }
    copied <- copied + 1L
  }
  cli::cli_alert_success("Template copied ({copied} items)")

  # ---- Step 2: Bundle portable R ----
  r_portable_dir <- fs::path(build_dir, "RPortable")

  if (.Platform$OS.type == "unix" && Sys.info()["sysname"] == "Darwin") {
    .deploy_bundle_r_macos(r_home, r_portable_dir)
  } else if (.Platform$OS.type == "windows") {
    .deploy_bundle_r_windows(r_home, r_portable_dir)
  } else {
    cli::cli_abort("Unsupported platform for R bundling: {.val {.Platform$OS.type}}")
  }

  # ---- Step 3: Populate library with work deps ----
  .deploy_populate_library(r_portable_dir)

  # ---- Step 4: Copy icon ----
  if (!is.null(icon)) {
    cli::cli_alert_info("Copying icon file...")
    icon_filename <- paste0("icon.", fs::path_ext(icon))
    fs::file_copy(icon, fs::path(build_dir, icon_filename), overwrite = TRUE)
    icon_rel <- icon_filename
    cli::cli_alert_success("Icon: {.file {icon_filename}}")
  }

  # ---- Step 4b: Copy header background image ----
  if (!is.null(header_background) && fs::file_exists(header_background)) {
    bg_filename <- paste0("header-bg.", fs::path_ext(header_background))
    renderer_dir <- fs::path(build_dir, "renderer")
    fs::dir_create(renderer_dir)
    fs::file_copy(header_background, fs::path(renderer_dir, bg_filename),
                  overwrite = TRUE)
    header_background <- bg_filename
    cli::cli_alert_success("Header background image: {.file renderer/{bg_filename}}")
  }

  # ---- Step 5: Write launcher.config.json ----
  cli::cli_alert_info("Writing launcher configuration...")
  .deploy_write_launcher_config(
    config_path = fs::path(build_dir, "launcher.config.json"),
    app_name = app_name,
    version = version,
    header_title = header_title,
    header_subtitle = header_subtitle,
    header_background = header_background,
    header_text = header_text,
    accent_color = accent_color,
    icon = icon_rel,
    file_extension = file_extension
  )
  cli::cli_alert_success("Config written")

  # ---- Step 6: Sync config to package.json ----
  .deploy_run_command("node", "sync-config.js", wd = build_dir,
              label = "node sync-config.js")

  # ---- Step 7: npm install ----
  cli::cli_alert_info("Installing Node.js dependencies (this may take a minute)...")
  .deploy_run_command("npm", "install", wd = build_dir, label = "npm install")

  # ---- Step 8: Build installers ----
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

  # ---- Step 9: Collect installers ----
  cli::cli_alert_info("Collecting installer files...")
  dist_dir <- fs::path(build_dir, "dist")

  if (!fs::dir_exists(dist_dir)) {
    cli::cli_abort("Build completed but no {.path dist/} directory found.")
  }

  all_files <- fs::dir_ls(dist_dir, recurse = FALSE)
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
