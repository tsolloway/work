#' @importFrom rlang %||%
NULL

#' Check prerequisites for building launchers
#'
#' Verifies that Node.js and npm are available on the system PATH. These are
#' required for building Electron-based desktop applications.
#'
#' @return Invisibly returns `TRUE` if all checks pass. Aborts with an
#'   informative error if any prerequisite is missing.
#'
#' @examples
#' \dontrun{
#' # Check that node and npm are available
#' deploy_check_prerequisites()
#' }
#'
#' @export
deploy_check_prerequisites <- function() {
  node_path <- Sys.which("node")
  if (node_path == "") {
    cli::cli_abort(c(
      "Node.js is not installed or not on PATH.",
      "i" = "Install Node.js from {.url https://nodejs.org/}"
    ))
  }
  node_version <- system2("node", "--version", stdout = TRUE, stderr = TRUE)
  cli::cli_alert_info("Node.js {node_version} found at {.path {node_path}}")

  npm_path <- Sys.which("npm")
  if (npm_path == "") {
    cli::cli_abort(c(
      "npm is not installed or not on PATH.",
      "i" = "npm should come with Node.js. Reinstall Node.js."
    ))
  }
  npm_version <- system2("npm", "--version", stdout = TRUE, stderr = TRUE)
  cli::cli_alert_info("npm {npm_version} found at {.path {npm_path}}")

  invisible(TRUE)
}


# ---- Internal helpers --------------------------------------------------------

#' Resolve a file path or URL to a local file
#'
#' If `value` is a URL (starts with `http://` or `https://`), downloads it to
#' a temporary file and returns the local path. Otherwise treats it as a local
#' file path (expanding `~`) and validates it exists.
#'
#' @param value Character. A local file path or URL.
#' @param label Character. Human-readable name for error messages (e.g.,
#'   `"icon"`, `"header_background"`).
#' @param valid_extensions Character vector or `NULL`. If non-NULL, validates
#'   that the resolved file has one of these extensions (lowercase, no dot).
#'
#' @return Character path to a local file.
#' @noRd
.deploy_resolve_file_or_url <- function(value, label = "file", valid_extensions = NULL) {
  if (grepl("^https?://", value)) {
    cli::cli_alert_info("Downloading {label} from URL...")
    url_path <- sub("\\?.*$", "", value)
    ext <- tolower(tools::file_ext(url_path))
    if (ext == "") ext <- "png"
    tmp <- tempfile(fileext = paste0(".", ext))
    tryCatch(
      utils::download.file(value, tmp, mode = "wb", quiet = TRUE),
      error = function(e) {
        cli::cli_abort(c(
          "Failed to download {label} from URL.",
          "x" = "URL: {.url {value}}",
          "i" = "{e$message}"
        ))
      }
    )
    cli::cli_alert_success("Downloaded {label} to {.path {tmp}}")
    value <- tmp
  } else {
    value <- fs::path_expand(value)
    if (!fs::file_exists(value)) {
      cli::cli_abort("{label} file not found: {.path {value}}")
    }
  }

  if (!is.null(valid_extensions)) {
    ext <- tolower(fs::path_ext(value))
    if (!ext %in% valid_extensions) {
      cli::cli_abort(c(
        "{label} must be a {.or {paste0('.', valid_extensions)}} file.",
        "x" = "Got: {.file .{ext}}"
      ))
    }
  }

  as.character(value)
}

#' Run a shell command with logging
#'
#' Wraps [system2()] with progress messages and error handling.
#'
#' @param cmd Character. The command to run.
#' @param args Character vector. Arguments to the command.
#' @param wd Character. Working directory to run the command in.
#' @param label Character. Human-readable label for progress messages.
#'   Defaults to the full command string.
#'
#' @return Invisibly returns the captured output. Aborts on non-zero exit.
#'
#' @noRd
.deploy_run_command <- function(cmd, args = character(), wd = ".", label = NULL) {
  label <- label %||% paste(cmd, paste(args, collapse = " "))
  cli::cli_alert_info("Running: {label}")

  result <- withr::with_dir(wd, {
    system2(
      cmd,
      args = args,
      stdout = TRUE,
      stderr = TRUE
    )
  })

  exit_code <- attr(result, "status") %||% 0L

  if (exit_code != 0L) {
    cli::cli_alert_danger("Command failed: {label}")
    message(paste(result, collapse = "\n"))
    cli::cli_abort("Command {.code {cmd}} exited with status {exit_code}.")
  }

  invisible(result)
}


#' Write launcher.config.json
#'
#' @param config_path Path where to write the file.
#' @param app_name,version,header_title,header_subtitle,header_background,accent_color,icon,file_extension
#'   The branding parameters.
#'
#' @noRd
.deploy_write_launcher_config <- function(config_path, app_name, version,
                                  header_title, header_subtitle,
                                  header_background, accent_color, icon,
                                  file_extension) {
  config <- list(
    appName = app_name,
    version = version,
    buildTimestamp = format(Sys.time(), "%Y%m%d%H%M%S"),
    headerTitle = header_title,
    headerSubtitle = header_subtitle,
    headerBackground = header_background,
    accentColor = accent_color,
    icon = icon,
    fileExtension = file_extension
  )
  jsonlite::write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)
}


#' Get the recommended default packages for a launcher
#'
#' Returns the full recursive dependency tree of the \pkg{work} package.
#' These are the packages that the launcher bundles in its `RPortable/library/`
#' directory. Used by [deploy_build_bundle()] to determine which packages
#' are already in the launcher (so bundles only need to include extras).
#'
#' @return A character vector of package names (alphabetical).
#'
#' @export
deploy_recommended_packages <- function() {
  pkgs <- tryCatch({
    db <- utils::installed.packages()
    deps <- tools::package_dependencies(
      "work", db = db, recursive = TRUE
    )[[1]]
    sort(unique(deps))
  }, error = function(e) NULL)

  if (is.null(pkgs) || length(pkgs) <= 50) {
    cli::cli_alert_warning(
      "Could not compute dynamic dependency tree \u2014 using hardcoded fallback"
    )
    pkgs <- .deploy_recommended_packages_fallback()
  }

  sort(unique(pkgs))
}


#' Hardcoded fallback for deploy_recommended_packages
#' @noRd
.deploy_recommended_packages_fallback <- function() {
  c(
    "abind", "askpass", "assertthat", "AzureAuth", "AzureGraph",
    "AzureRMR", "AzureStor", "backports", "base64enc", "bit",
    "bit64", "boot", "brew", "brio", "broom", "bslib",
    "ca", "cachem", "callr", "car", "carData", "caret",
    "cellranger", "checkmate", "class", "classInt", "cli", "clipr",
    "clock", "cluster", "codetools", "colorspace", "combinat",
    "commonmark", "corrplot", "corrr", "cowplot", "cpp11", "crayon",
    "credentials", "crosstalk", "curl", "data.table", "DBI",
    "Deriv", "desc", "devtools", "diagram", "diffobj", "digest",
    "doBy", "downlit", "dplyr", "DT", "e1071", "ellipsis",
    "entropy", "evaluate", "fansi", "farver", "fastmap",
    "fontawesome", "forcats", "foreach", "forecast", "foreign",
    "Formula", "fracdiff", "fs", "furrr", "future", "future.apply",
    "gclus", "generics", "gert", "ggplot2", "ggrepel", "gh",
    "gitcreds", "globals", "glue", "gower", "GPArotation",
    "graphics", "grDevices", "grid", "gridExtra", "gsubfn",
    "gtable", "hardhat", "haven", "highcharter", "highr", "Hmisc",
    "hms", "htmlTable", "htmltools", "htmlwidgets", "httpuv",
    "httr", "httr2", "igraph", "ini", "ipred", "isoband",
    "iterators", "janitor", "jose", "jquerylib", "jsonlite",
    "KernSmooth", "klaR", "knitr", "labeling", "labelled", "later",
    "lattice", "lava", "lazyeval", "lifecycle", "listenv", "lme4",
    "lmtest", "lubridate", "magrittr", "MASS", "Matrix",
    "MatrixModels", "memoise", "methods", "mgcv",
    "microbenchmark", "mime", "miniUI", "minqa", "mnormt",
    "ModelMetrics", "modelr", "multcomp", "mvtnorm", "nlme",
    "nloptr", "nnet", "numDeriv", "openssl", "openxlsx",
    "openxlsx2", "otel", "parallel", "parallelly",
    "pbkrtest", "permute", "pillar", "pkgbuild", "pkgconfig",
    "pkgdown", "pkgload", "plotly", "plyr", "polspline", "praise",
    "prettyunits", "pROC", "processx", "prodlim", "profvis",
    "progress", "progressr", "promises", "proto", "proxy", "ps",
    "psych", "purrr", "qap", "quantmod", "quantreg", "questionr",
    "R.cache", "R.methodsS3", "R.oo", "R.utils", "R6", "ragg",
    "rappdirs", "rbibutils", "rcmdcheck", "RColorBrewer", "Rcpp",
    "RcppArmadillo", "RcppEigen", "Rdpack", "readr", "readxl",
    "recipes", "reformulas", "registry", "rematch", "remotes",
    "repr", "reshape2", "rjson", "rlang", "rlist", "rmarkdown",
    "rms", "roxygen2", "rpart", "rprojroot", "rstudioapi",
    "rversions", "rvest", "S7", "sandwich", "sass", "scales",
    "selectr", "seriation", "sessioninfo", "shape", "shiny",
    "skimr", "snakecase", "sourcetools", "SparseM", "sparsevctrs",
    "splines", "SQUAREM", "stats", "stats4", "stringi", "stringr",
    "styler", "survival", "sys", "systemfonts", "testthat",
    "textshaping", "TH.data", "tibble", "tictoc", "tidyr",
    "tidyselect", "timechange", "timeDate", "tinytex", "tools",
    "TSP", "TTR", "tzdb", "urca", "urlchecker", "usethis", "utf8",
    "utils", "uuid", "vctrs", "vegan", "viridisLite", "visNetwork",
    "vroom", "waldo", "whisker", "withr", "xfun", "XML",
    "xml2", "xopen", "xtable", "xts", "yaml", "zip", "zoo"
  )
}


# ---- Portable R bundling helpers ---------------------------------------------

#' Bundle macOS R installation into a portable directory
#'
#' Copies the R installation from `r_home` into `dest_dir` as a portable
#' `RPortable/` directory. On macOS, R is typically installed as a framework
#' at `/Library/Frameworks/R.framework/Versions/Current/Resources/`.
#'
#' @param r_home Character. Path to R home (from `R.home()`).
#' @param dest_dir Character. Path where `RPortable/` will be created.
#' @noRd
.deploy_bundle_r_macos <- function(r_home, dest_dir) {
  cli::cli_alert_info("Bundling R for macOS...")

  if (!fs::dir_exists(r_home)) {
    cli::cli_abort("R home not found: {.path {r_home}}")
  }

  # Verify essential subdirectories exist
  required_dirs <- c("bin", "lib", "library", "etc", "share")
  for (d in required_dirs) {
    if (!fs::dir_exists(fs::path(r_home, d))) {
      cli::cli_abort("R installation missing {.path {d}/} directory at {.path {r_home}}")
    }
  }

  cli::cli_alert_info("Copying R from {.path {r_home}}...")
  fs::dir_create(dest_dir)

  # Copy essential directories
  dirs_to_copy <- c("bin", "lib", "etc", "share", "library", "include",
                     "modules", "doc")

  for (d in dirs_to_copy) {
    src <- fs::path(r_home, d)
    if (fs::dir_exists(src)) {
      fs::dir_copy(src, fs::path(dest_dir, d))
    }
  }

  # Copy top-level files (e.g., COPYING, SVN-REVISION)
  top_files <- fs::dir_ls(r_home, type = "file")
  for (f in top_files) {
    fs::file_copy(f, fs::path(dest_dir, fs::path_file(f)))
  }

  # Strip base R packages of fat (help, html, doc, tests) to save space
  base_lib <- fs::path(dest_dir, "library")
  if (fs::dir_exists(base_lib)) {
    base_pkgs <- fs::dir_ls(base_lib, type = "directory")
    for (pkg_dir in base_pkgs) {
      .deploy_strip_package_fat(pkg_dir)
    }
  }

  # Compute size
  r_size <- sum(fs::file_info(fs::dir_ls(dest_dir, recurse = TRUE, type = "file"))$size,
                na.rm = TRUE)
  cli::cli_alert_success(
    "Bundled R for macOS ({(.deploy_format_size(r_size))})"
  )
}


#' Bundle Windows R installation into a portable directory
#'
#' Copies the R installation from `r_home` into `dest_dir` as a portable
#' `RPortable/` directory.
#'
#' @param r_home Character. Path to R home (from `R.home()`).
#' @param dest_dir Character. Path where `RPortable/` will be created.
#' @noRd
.deploy_bundle_r_windows <- function(r_home, dest_dir) {
  cli::cli_alert_info("Bundling R for Windows...")

  if (!fs::dir_exists(r_home)) {
    cli::cli_abort("R home not found: {.path {r_home}}")
  }

  cli::cli_alert_info("Copying R from {.path {r_home}}...")
  fs::dir_create(dest_dir)

  # Copy essential directories
  dirs_to_copy <- c("bin", "etc", "share", "library", "include",
                     "modules", "doc", "Tcl")

  for (d in dirs_to_copy) {
    src <- fs::path(r_home, d)
    if (fs::dir_exists(src)) {
      fs::dir_copy(src, fs::path(dest_dir, d))
    }
  }

  # Copy top-level files
  top_files <- fs::dir_ls(r_home, type = "file")
  for (f in top_files) {
    fs::file_copy(f, fs::path(dest_dir, fs::path_file(f)))
  }

  # Strip base R packages of fat
  base_lib <- fs::path(dest_dir, "library")
  if (fs::dir_exists(base_lib)) {
    base_pkgs <- fs::dir_ls(base_lib, type = "directory")
    for (pkg_dir in base_pkgs) {
      .deploy_strip_package_fat(pkg_dir)
    }
  }

  r_size <- sum(fs::file_info(fs::dir_ls(dest_dir, recurse = TRUE, type = "file"))$size,
                na.rm = TRUE)
  cli::cli_alert_success(
    "Bundled R for Windows ({(.deploy_format_size(r_size))})"
  )
}


#' Populate the RPortable library with work package dependencies
#'
#' Copies all recursive dependencies of the `work` package from the local
#' R library into `RPortable/library/`. Also copies the `work` package itself.
#' Each copied package is stripped of non-essential files (help, html, doc,
#' tests) to reduce size.
#'
#' @param r_portable_dir Character. Path to the RPortable directory.
#' @noRd
.deploy_populate_library <- function(r_portable_dir) {
  lib_dest <- fs::path(r_portable_dir, "library")
  fs::dir_create(lib_dest)

  # Get all work dependencies
  cli::cli_alert_info("Computing work package dependency tree...")
  db <- utils::installed.packages()
  deps <- tools::package_dependencies("work", db = db, recursive = TRUE)[[1]]

  # Include work itself
  all_pkgs <- unique(c("work", deps))

  cli::cli_alert_info("Copying {length(all_pkgs)} package(s) to RPortable/library/...")

  copied <- 0L
  skipped <- 0L

  for (pkg in all_pkgs) {
    # Skip if already in the base R library we just copied
    dest_pkg <- fs::path(lib_dest, pkg)
    if (fs::dir_exists(dest_pkg)) {
      skipped <- skipped + 1L
      next
    }

    # Find the package in local .libPaths()
    pkg_path <- tryCatch(
      find.package(pkg, quiet = TRUE),
      error = function(e) character()
    )
    if (length(pkg_path) == 0) {
      cli::cli_alert_warning("Package {.pkg {pkg}} not found locally, skipping")
      next
    }

    fs::dir_copy(pkg_path, dest_pkg)
    .deploy_strip_package_fat(dest_pkg)
    copied <- copied + 1L
  }

  # Compute total library size
  lib_size <- sum(
    fs::file_info(fs::dir_ls(lib_dest, recurse = TRUE, type = "file"))$size,
    na.rm = TRUE
  )
  cli::cli_alert_success(
    "Library populated: {copied} copied, {skipped} already present ({(.deploy_format_size(lib_size))})"
  )
}


#' Strip non-essential files from a package directory
#'
#' Removes help/, html/, doc/, tests/, and demo/ directories from a package
#' to reduce the size of the bundled R library. Typically saves 40-60% of
#' the package size.
#'
#' @param pkg_dir Character. Path to the installed package directory.
#' @noRd
.deploy_strip_package_fat <- function(pkg_dir) {
  dirs_to_remove <- c("help", "html", "doc", "tests", "demo",
                       "examples", "vignettes")
  for (d in dirs_to_remove) {
    target <- fs::path(pkg_dir, d)
    if (fs::dir_exists(target)) {
      fs::dir_delete(target)
    }
  }
}
