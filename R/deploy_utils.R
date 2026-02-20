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
  # Check node

  node_path <- Sys.which("node")
  if (node_path == "") {
    cli::cli_abort(c(
      "Node.js is not installed or not on PATH.",
      "i" = "Install Node.js from {.url https://nodejs.org/}"
    ))
  }
  node_version <- system2("node", "--version", stdout = TRUE, stderr = TRUE)
  cli::cli_alert_info("Node.js {node_version} found at {.path {node_path}}")

  # Check npm
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


#' Update the bundled shinylive runtime
#'
#' Refreshes the shinylive/webR runtime files stored in the package's
#' `inst/deploy_template/runtime/` directory from the shinylive R package cache.
#' Run this after updating the shinylive R package to keep the launcher
#' runtime in sync.
#'
#' The function locates the shinylive cache (populated by
#' [shinylive::export()]), then assembles an R-only runtime by:
#' \itemize{
#'   \item Rendering the mustache `index.html` template into concrete HTML
#'   \item Copying `shinylive-sw.js` and `edit/` directory
#'   \item Copying `shinylive/` (excluding Python-specific files: `pyodide/`,
#'     `pyright/`, `examples.json`)
#' }
#'
#' @param package_dir Character. Path to the work package source
#'   directory. Defaults to the current working directory (assumes you're in
#'   the package root).
#'
#' @return Invisibly returns the path to the updated runtime directory.
#'
#' @details
#' The shinylive cache is located at:
#' \itemize{
#'   \item macOS: `~/Library/Caches/shinylive/shinylive-{version}/`
#'   \item Linux: `~/.cache/shinylive/shinylive-{version}/`
#'   \item Windows: `%LOCALAPPDATA%/Cache/shinylive/shinylive-{version}/`
#' }
#'
#' If the cache does not exist, run `shinylive::export()` once on any Shiny
#' app to download and cache the assets.
#'
#' @examples
#' \dontrun{
#' # From the package root directory:
#' deploy_update_runtime()
#'
#' # Or specify the package path explicitly:
#' deploy_update_runtime(package_dir = "~/Documents/GitHub/work")
#' }
#'
#' @export
deploy_update_runtime <- function(package_dir = ".") {
  if (!requireNamespace("shinylive", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg shinylive} package is required for {.fn deploy_update_runtime}.",
      "i" = "Install it with: {.code install.packages('shinylive')}"
    ))
  }

  package_dir <- fs::path_expand(package_dir)

  # Support both source packages (inst/deploy_template/) and installed packages (deploy_template/)
  if (fs::dir_exists(fs::path(package_dir, "inst", "deploy_template"))) {
    runtime_dest <- fs::path(package_dir, "inst", "deploy_template", "runtime")
  } else if (fs::dir_exists(fs::path(package_dir, "deploy_template"))) {
    runtime_dest <- fs::path(package_dir, "deploy_template", "runtime")
  } else {
    cli::cli_abort(c(
      "Could not find {.path deploy_template/} in {.path {package_dir}}.",
      "i" = "Make sure you're pointing to the work package root."
    ))
  }

  # Find the shinylive cache
  cache_dir <- .deploy_find_shinylive_cache()

  cli::cli_alert_info("Shinylive cache found at: {.path {cache_dir}}")

  # Remove old runtime

  if (fs::dir_exists(runtime_dest)) {
    cli::cli_alert_info("Removing old runtime...")
    fs::dir_delete(runtime_dest)
  }
  fs::dir_create(runtime_dest)

  # Assemble new runtime from cache
  .deploy_assemble_runtime_from_cache(cache_dir, runtime_dest)

  size <- system2("du", c("-sh", runtime_dest), stdout = TRUE)
  cli::cli_alert_success("Runtime updated: {size}")


  invisible(as.character(runtime_dest))
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
    # Determine extension from URL (strip query params)
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
#' @param app_name,header_title,header_subtitle,header_background,accent_color,icon,file_extension
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
    headerTitle = header_title,
    headerSubtitle = header_subtitle,
    headerBackground = header_background,
    accentColor = accent_color,
    icon = icon,
    fileExtension = file_extension
  )
  jsonlite::write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE)
}


#' Find the shinylive cache directory
#'
#' @return Character path to the shinylive cache directory.
#' @noRd
.deploy_find_shinylive_cache <- function() {
  # Determine cache base directory (platform-specific)
  cache_base <- Sys.getenv("XDG_CACHE_HOME", unset = "")
  if (cache_base == "") {
    sysname <- Sys.info()[["sysname"]]
    if (sysname == "Darwin") {
      cache_base <- fs::path(Sys.getenv("HOME"), "Library", "Caches")
    } else if (sysname == "Windows") {
      cache_base <- Sys.getenv("LOCALAPPDATA")
      if (cache_base == "") {
        cache_base <- fs::path(Sys.getenv("USERPROFILE"), "AppData", "Local")
      }
      cache_base <- fs::path(cache_base, "Cache")
    } else {
      cache_base <- fs::path(Sys.getenv("HOME"), ".cache")
    }
  }

  shinylive_base <- fs::path(cache_base, "shinylive")

  if (!fs::dir_exists(shinylive_base)) {
    cli::cli_abort(c(
      "Shinylive cache not found at {.path {shinylive_base}}.",
      "i" = "Run {.code shinylive::export()} once on any Shiny app to download and cache the assets."
    ))
  }

  # Find the versioned subdirectory (e.g., shinylive-0.9.1)
  versions <- fs::dir_ls(shinylive_base, type = "directory")
  if (length(versions) == 0) {
    cli::cli_abort(c(
      "Shinylive cache is empty at {.path {shinylive_base}}.",
      "i" = "Run {.code shinylive::export()} once to download assets."
    ))
  }

  # Use the most recent version (last alphabetically)
  cache_dir <- sort(versions, decreasing = TRUE)[[1]]

  # Validate structure
  required <- c("export_template", "shinylive", "shinylive-sw.js")
  missing <- required[!fs::file_exists(fs::path(cache_dir, required)) &
                      !fs::dir_exists(fs::path(cache_dir, required))]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Shinylive cache at {.path {cache_dir}} is missing: {.file {missing}}",
      "i" = "Try re-running {.code shinylive::export()} to refresh the cache."
    ))
  }

  as.character(cache_dir)
}


#' Assemble runtime directory from shinylive cache
#'
#' @param cache_dir Path to the shinylive cache (e.g., shinylive-0.9.1/)
#' @param dest_dir Path where runtime/ will be created
#' @noRd
.deploy_assemble_runtime_from_cache <- function(cache_dir, dest_dir) {
  fs::dir_create(dest_dir)

  # 1. Render index.html from mustache template
  cli::cli_alert_info("Rendering index.html from template...")
  template_path <- fs::path(cache_dir, "export_template", "index.html")
  template <- readLines(template_path, warn = FALSE)
  html <- paste(template, collapse = "\n")

  # Simple mustache replacement for known variables
  html <- gsub("\\{\\{#title\\}\\}.*?\\{\\{/title\\}\\}", "<title>Shiny App</title>", html)
  html <- gsub("\\{\\{REL_PATH\\}\\}", "", html)
  html <- gsub("\\{\\{APP_ENGINE\\}\\}", "r", html)
  html <- gsub("\\{\\{\\{ include_in_head \\}\\}\\}", "", html)
  html <- gsub("\\{\\{\\{ include_before_body \\}\\}\\}", "", html)
  html <- gsub("\\{\\{\\{ include_after_body \\}\\}\\}", "", html)

  writeLines(html, fs::path(dest_dir, "index.html"))

  # 2. Copy edit/ directory from export_template
  edit_src <- fs::path(cache_dir, "export_template", "edit")
  if (fs::dir_exists(edit_src)) {
    cli::cli_alert_info("Copying edit/ directory...")
    fs::dir_copy(edit_src, fs::path(dest_dir, "edit"))
  }

  # 3. Copy shinylive-sw.js
  sw_src <- fs::path(cache_dir, "shinylive-sw.js")
  if (fs::file_exists(sw_src)) {
    cli::cli_alert_info("Copying shinylive-sw.js...")
    fs::file_copy(sw_src, fs::path(dest_dir, "shinylive-sw.js"))
  }

  # 4. Copy shinylive/ (R-only: exclude pyodide, pyright, examples.json)
  cli::cli_alert_info("Copying shinylive/ (R-only subset, excluding pyodide/pyright)...")
  shinylive_src <- fs::path(cache_dir, "shinylive")
  shinylive_dest <- fs::path(dest_dir, "shinylive")
  fs::dir_create(shinylive_dest)

  exclude <- c("pyodide", "pyright", "examples.json")
  entries <- fs::dir_ls(shinylive_src)

  for (entry in entries) {
    entry_name <- fs::path_file(entry)
    if (!entry_name %in% exclude) {
      if (fs::is_dir(entry)) {
        fs::dir_copy(entry, fs::path(shinylive_dest, entry_name))
      } else {
        fs::file_copy(entry, fs::path(shinylive_dest, entry_name))
      }
    }
  }
}


#' Get the recommended default packages for a launcher
#'
#' Returns a curated character vector of R package names commonly used in Shiny
#' apps. When passed to [deploy_launcher()] via `include_recommended_packages = TRUE`,
#' these packages are pre-loaded into the launcher's runtime so that bundles
#' built with [deploy_build_bundle()] don't need to include them — dramatically
#' reducing bundle size.
#'
#' The list is intentionally kept in one place so it's easy to edit. To add
#' custom packages on top of the recommended set, combine them:
#' ```
#' deploy_build_bundle(
#'   ...,
#'   default_packages = c(deploy_recommended_packages(), "my_extra_pkg")
#' )
#' ```
#'
#' @return A character vector of package names (alphabetical).
#'
#' @export
deploy_recommended_packages <- function() {
  c(
    "AzureStor", "bnlearn", "bslib", "car", "caret", "cli",
    "cluster", "corrplot", "corrr", "DBI", "devtools",
    "digest", "dplyr", "DT", "entropy", "fs", "furrr", "future",
    "future.apply", "glue", "gsubfn", "haven", "highcharter",
    "Hmisc", "igraph", "janitor", "jsonlite", "klaR", "knitr",
    "lubridate", "magrittr", "mclust", "openxlsx", "openxlsx2",
    "pak", "parallelly", "plotly", "psych", "purrr", "RColorBrewer",
    "readxl", "rlang", "rmarkdown", "rms", "rvest",
    "scales", "shiny", "shinylive", "skimr", "stringi", "stringr",
    "tibble", "tictoc", "usethis", "uuid", "visNetwork", "withr",
    "work", "xfun", "zip"
  )
}


# ---- Package resolution cache ------------------------------------------------

#' Get cache directory for resolved default packages
#' @return Character path to the cache directory.
#' @noRd
.deploy_get_resolved_packages_cache_dir <- function() {
  cache_base <- tools::R_user_dir("resondex.deploy", which = "cache")
  cache_dir <- fs::path(cache_base, "resolved_packages")
  fs::dir_create(cache_dir, recurse = TRUE)
  as.character(cache_dir)
}


#' Compute cache key for a set of packages
#'
#' Combines sorted package names with the shinylive version so the cache
#' invalidates when either changes.
#'
#' @param packages Character vector of package names.
#' @return Character hash string.
#' @noRd
.deploy_compute_cache_key <- function(packages) {
  sorted_pkgs <- sort(packages)
  shinylive_version <- tryCatch(
    as.character(utils::packageVersion("shinylive")),
    error = function(e) "unknown"
  )
  cache_input <- paste(c(sorted_pkgs, shinylive_version), collapse = "|")
  digest::digest(cache_input, algo = "xxhash64")
}


#' Check if cached resolution exists and is valid
#'
#' @param cache_key Character cache key hash.
#' @return Character path to cached packages dir if valid, `NULL` otherwise.
#' @noRd
.deploy_check_cache <- function(cache_key) {
  cache_base <- .deploy_get_resolved_packages_cache_dir()
  cache_path <- fs::path(cache_base, cache_key)

  if (!fs::dir_exists(cache_path)) return(NULL)

  pkg_dir <- fs::path(cache_path, "packages")
  if (!fs::dir_exists(pkg_dir)) return(NULL)

  tgz_files <- list.files(as.character(pkg_dir), pattern = "\\.tgz$",
                           recursive = TRUE)
  if (length(tgz_files) == 0) return(NULL)

  as.character(cache_path)
}


#' Save resolved packages to cache
#'
#' @param cache_key Character cache key hash.
#' @param packages_dir Path to the packages directory to cache.
#' @return Character path to the cached location.
#' @noRd
.deploy_save_to_cache <- function(cache_key, packages_dir) {
  cache_base <- .deploy_get_resolved_packages_cache_dir()
  cache_path <- fs::path(cache_base, cache_key)

  fs::dir_create(cache_path)
  dest_pkg_dir <- fs::path(cache_path, "packages")
  if (fs::dir_exists(dest_pkg_dir)) {
    fs::dir_delete(dest_pkg_dir)
  }
  fs::dir_copy(packages_dir, dest_pkg_dir)

  cli::cli_alert_success("Cached resolved packages for future use")
  as.character(cache_path)
}


# ---- Resolve default packages ------------------------------------------------

#' Resolve default packages via shinylive export (with caching)
#'
#' Creates a dummy Shiny app that loads the specified packages, runs
#' [shinylive::export()] to resolve all dependencies and download
#' WebAssembly-compiled `.tgz` files, then returns the path to the
#' packages directory. Results are cached persistently so subsequent
#' calls with the same package list return instantly.
#'
#' @param packages Character vector of R package names.
#' @return A list with three elements:
#'   \describe{
#'     \item{export_dir}{Path to the export directory. When `cached = FALSE`,
#'       the caller should clean up via `on.exit(fs::dir_delete(...))`.
#'       When `cached = TRUE`, this points to the persistent cache and
#'       must **not** be deleted.}
#'     \item{packages_dir}{Path to the directory containing `.tgz` files
#'       and `metadata.rds`.}
#'     \item{cached}{Logical. `TRUE` if the result was served from cache.}
#'   }
#' @noRd
.deploy_resolve_default_packages <- function(packages) {
  if (!requireNamespace("shinylive", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg shinylive} package is required to resolve default packages.",
      "i" = "Install it with: {.code install.packages('shinylive')}"
    ))
  }

  # ---- Check cache first ----
  cache_key <- .deploy_compute_cache_key(packages)
  cached_path <- .deploy_check_cache(cache_key)

  if (!is.null(cached_path)) {
    pkg_dir <- fs::path(cached_path, "packages")
    tgz_files <- list.files(as.character(pkg_dir), pattern = "\\.tgz$",
                             recursive = TRUE)
    cli::cli_alert_success(
      "Using cached resolution for {length(packages)} default package(s) ({length(tgz_files)} .tgz files)"
    )
    return(list(
      export_dir = as.character(cached_path),
      packages_dir = as.character(pkg_dir),
      cached = TRUE
    ))
  }

  # ---- Cache miss — resolve via shinylive::export() ----
  cli::cli_alert_info(
    "Resolving {length(packages)} default package(s) and dependencies via shinylive..."
  )

  dummy_app_dir <- tempfile(pattern = "default-pkgs-app-")
  fs::dir_create(dummy_app_dir)
  on.exit(fs::dir_delete(dummy_app_dir), add = TRUE)

  library_lines <- paste0("library(", packages, ")")
  app_code <- c(
    library_lines,
    "",
    "ui <- shiny::fluidPage(shiny::h1('default packages'))",
    "server <- function(input, output, session) {}",
    "shiny::shinyApp(ui, server)"
  )
  writeLines(app_code, fs::path(dummy_app_dir, "app.R"))

  export_dir <- tempfile(pattern = "default-pkgs-export-")
  fs::dir_create(export_dir)

  shinylive::export(dummy_app_dir, export_dir, quiet = TRUE)
  cli::cli_alert_success("Default packages resolved")

  pkg_dir <- fs::path(export_dir, "shinylive", "webr", "packages")
  if (!fs::dir_exists(pkg_dir)) {
    fs::dir_delete(export_dir)
    cli::cli_abort(
      "shinylive::export() did not produce a packages directory for the default packages list."
    )
  }

  tgz_files <- list.files(as.character(pkg_dir), pattern = "\\.tgz$",
                           recursive = TRUE)
  if (length(tgz_files) == 0) {
    fs::dir_delete(export_dir)
    cli::cli_abort(
      "shinylive::export() did not produce any package .tgz files for the default packages list."
    )
  }

  cli::cli_alert_info(
    "Resolved {length(tgz_files)} package .tgz file(s) (including dependencies)"
  )

  # Save to cache for future use
  .deploy_save_to_cache(cache_key, pkg_dir)

  list(
    export_dir = as.character(export_dir),
    packages_dir = as.character(pkg_dir),
    cached = FALSE
  )
}


#' Replace library(tidyverse) with only the sub-packages actually used
#'
#' Scans all R files to detect which tidyverse sub-packages are actually
#' needed (by checking for function calls and explicit library() calls),
#' then replaces `library(tidyverse)` with individual library() calls.
#' This dramatically reduces bundle size since shinylive only downloads
#' packages that are explicitly loaded.
#'
#' @param r_files Character vector of R file paths to scan and patch.
#' @noRd
.deploy_expand_tidyverse <- function(r_files) {
  # Read all code across all files
  all_code <- character()
  tidyverse_file <- NULL
  tidyverse_line_idx <- NULL

  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    all_code <- c(all_code, lines)
    # Find the file and line containing library(tidyverse)
    tv_idx <- grep("^\\s*(library|require)\\s*\\(\\s*tidyverse\\s*\\)", lines)
    if (length(tv_idx) > 0 && is.null(tidyverse_file)) {
      tidyverse_file <- f
      tidyverse_line_idx <- tv_idx[1]
    }
  }

  if (is.null(tidyverse_file)) return(invisible(NULL))

  # Collapse all code into one string for pattern matching
  code_text <- paste(all_code, collapse = "\n")

  # Map of tidyverse sub-packages to signature functions/patterns
  # that indicate the package is actually used
  tidyverse_pkgs <- list(
    ggplot2  = c("ggplot", "aes", "geom_", "scale_", "theme", "coord_",
                 "facet_", "labs", "ggtitle", "xlab", "ylab", "qplot",
                 "stat_", "position_", "annotation_", "element_"),
    dplyr    = c("mutate", "filter", "select", "arrange", "summarise",
                 "summarize", "group_by", "ungroup", "count", "tally",
                 "rename", "relocate", "slice", "pull", "distinct",
                 "left_join", "right_join", "inner_join", "full_join",
                 "anti_join", "semi_join", "bind_rows", "bind_cols",
                 "across", "if_else", "case_when", "n\\(\\)",
                 "between", "lag", "lead", "row_number", "ntile",
                 "starts_with", "ends_with", "contains", "matches",
                 "everything", "where", "any_of", "all_of"),
    tidyr    = c("pivot_longer", "pivot_wider", "spread", "gather",
                 "separate", "unite", "nest", "unnest", "fill",
                 "drop_na", "replace_na", "complete", "expand",
                 "crossing", "nesting"),
    readr    = c("read_csv", "read_tsv", "read_delim", "read_fwf",
                 "read_table", "read_rds", "write_csv", "write_tsv",
                 "write_rds", "parse_number", "parse_date", "col_types",
                 "cols\\("),
    purrr    = c("map\\(", "map_chr", "map_dbl", "map_int", "map_lgl",
                 "map_df", "map2", "pmap", "walk", "imap", "keep",
                 "discard", "compact", "reduce", "accumulate",
                 "possibly", "safely", "quietly", "list_rbind",
                 "list_c", "pluck"),
    tibble   = c("tibble\\(", "tribble", "as_tibble", "enframe",
                 "deframe", "add_row", "add_column"),
    stringr  = c("str_detect", "str_replace", "str_extract", "str_split",
                 "str_sub", "str_trim", "str_pad", "str_c\\(",
                 "str_count", "str_length", "str_to_lower",
                 "str_to_upper", "str_remove", "str_wrap",
                 "str_locate", "str_match", "str_glue",
                 "str_starts", "str_ends", "str_squish"),
    forcats  = c("fct_reorder", "fct_relevel", "fct_infreq",
                 "fct_lump", "fct_recode", "fct_rev", "fct_collapse",
                 "fct_drop", "fct_count", "fct_other"),
    lubridate = c("ymd", "mdy", "dmy", "ymd_hms", "mdy_hms",
                  "dmy_hms", "now\\(", "today\\(",
                  "year\\(", "month\\(", "day\\(", "hour\\(",
                  "minute\\(", "second\\(", "wday\\(", "yday\\(",
                  "date\\(", "as_date", "as_datetime",
                  "duration", "period", "interval",
                  "floor_date", "ceiling_date", "round_date",
                  "with_tz", "force_tz", "parse_date_time")
  )

  # Also check for explicit library() calls for these packages
  detected <- character()
  for (pkg in names(tidyverse_pkgs)) {
    # Check if already loaded explicitly via library(pkg)
    lib_pattern <- paste0("(library|require)\\s*\\(\\s*", pkg, "\\s*\\)")
    if (grepl(lib_pattern, code_text)) {
      detected <- c(detected, pkg)
      next
    }
    # Check if any signature functions are used
    patterns <- tidyverse_pkgs[[pkg]]
    for (pat in patterns) {
      if (grepl(pat, code_text)) {
        detected <- c(detected, pkg)
        break
      }
    }
  }

  # Always keep at least the core if nothing detected
  if (length(detected) == 0) {
    detected <- c("ggplot2", "dplyr", "tidyr", "readr")
  }
  detected <- unique(detected)

  # Build replacement lines
  # Remove any packages that are already loaded via their own library() call
  # in any file — we don't want duplicate library() calls
  already_loaded <- character()
  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    for (pkg in detected) {
      pat <- paste0("^\\s*(library|require)\\s*\\(\\s*", pkg, "\\s*\\)")
      if (any(grepl(pat, lines))) {
        already_loaded <- c(already_loaded, pkg)
      }
    }
  }
  to_add <- setdiff(detected, already_loaded)

  replacement <- paste0("library(", to_add, ")")
  replacement_text <- paste(replacement, collapse = "\n")

  # Replace library(tidyverse) in ALL R files (not just the first one found)
  tv_pattern <- "^\\s*(library|require)\\s*\\(\\s*tidyverse\\s*\\)"
  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    tv_idx <- grep(tv_pattern, lines)
    if (length(tv_idx) == 0) next
    # Replace the first occurrence with the individual library() calls,
    # and comment out any subsequent ones (shouldn't happen, but safe)
    lines[tv_idx[1]] <- replacement_text
    if (length(tv_idx) > 1) {
      for (idx in tv_idx[-1]) {
        lines[idx] <- paste0("# [shinylive-patch] ", lines[idx])
      }
    }
    writeLines(lines, f)
  }

  all_pkgs <- paste(detected, collapse = ", ")
  removed_pkgs <- setdiff(
    c("ggplot2", "dplyr", "tidyr", "readr", "purrr", "tibble",
      "stringr", "forcats", "lubridate"),
    detected
  )

  cli::cli_alert_info(
    "Expanded library(tidyverse) -> {length(detected)} package(s): {all_pkgs}"
  )
  if (length(removed_pkgs) > 0) {
    cli::cli_alert_success(
      "Dropped unused: {paste(removed_pkgs, collapse = ', ')}"
    )
  }

  invisible(detected)
}


#' Patch R files for Shinylive compatibility
#'
#' Scans all `.R` files in a directory and applies automatic fixes for
#' patterns that are incompatible with Shinylive/WebR. All patches are
#' applied to the **copy** in the clean temp directory, never the original
#' source files.
#'
#' ## Patches applied
#'
#' \describe{
#'   \item{tidyverse expansion}{`library(tidyverse)` is replaced with
#'     individual `library()` calls for only the sub-packages actually
#'     used in the app code. This prevents shinylive from downloading
#'     the entire tidyverse ecosystem (~100 packages, ~60+ MB).}
#'   \item{future plan}{`plan(multiprocess)`, `plan(multisession)`,
#'     `plan(multicore)` are replaced with `future::plan(sequential)`.
#'     WebR is single-threaded so async plans cannot work.}
#'   \item{Disk caching}{`shinyOptions(cache = diskCache(...))` is
#'     commented out. WebR has no persistent filesystem for caching.}
#'   \item{renderCachedPlot}{Replaced with `renderPlot`. The
#'     `cacheKeyExpr` argument is removed since `renderPlot` does not
#'     accept it.}
#'   \item{saveRDS / write.csv / write_csv / write_rds / write.table}{
#'     Lines containing file-write calls are commented out. WebR cannot
#'     write to the filesystem.}
#'   \item{Remote URL data reads}{Calls like
#'     `read_csv("http://example.com/data.csv")` are detected at build
#'     time. The remote file is downloaded to the app's `data/` directory
#'     and the code is rewritten to read from the local path.}
#' }
#'
#' @param app_dir Character. Path to the app directory containing `.R` files.
#'   Files are modified in place (this should be the temp copy, not the
#'   original source).
#' @return Invisibly returns a character vector of patched file paths
#'   (only files that were actually modified).
#' @noRd
.deploy_patch_shinylive_compat <- function(app_dir) {
  r_files <- list.files(app_dir, pattern = "\\.[Rr]$", recursive = TRUE,
                        full.names = TRUE)
  if (length(r_files) == 0) return(invisible(character()))

  # ---- Pre-pass: expand library(tidyverse) into specific packages ----
  .deploy_expand_tidyverse(r_files)

  patched <- character()
  patches_applied <- list()

  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    original <- lines
    fname <- fs::path_file(f)
    file_patches <- character()

    # ---- 1. future plan: force sequential ----
    new_lines <- gsub(
      "(future::)?plan\\s*\\(\\s*(multiprocess|multisession|multicore)\\s*\\)",
      "future::plan(sequential)",
      lines
    )
    if (!identical(new_lines, lines)) {
      file_patches <- c(file_patches, "future plan -> sequential")
      lines <- new_lines
    }

    # ---- 2. diskCache: comment out shinyOptions(cache = diskCache(...)) ----
    disk_cache_idx <- grep("diskCache\\s*\\(", lines)
    if (length(disk_cache_idx) > 0) {
      for (idx in disk_cache_idx) {
        if (!grepl("^\\s*#", lines[idx])) {
          lines[idx] <- paste0("# [shinylive-patch] ", lines[idx])
        }
      }
      file_patches <- c(file_patches, "diskCache commented out")
    }

    # ---- 3. renderCachedPlot -> renderPlot ----
    cached_plot_idx <- grep("renderCachedPlot\\s*\\(", lines)
    if (length(cached_plot_idx) > 0) {
      lines <- gsub("renderCachedPlot\\s*\\(", "renderPlot(", lines)
      # Remove cacheKeyExpr argument. The tricky part is that the closing )
      # for renderCachedPlot() often sits at the end of the cacheKeyExpr line:
      #   },                             <- trailing comma needs removing
      #   cacheKeyExpr = { input$year }) <- ) closes renderCachedPlot
      # We need to: remove comma from prior line, and replace the
      # cacheKeyExpr line with just the closing ")"
      cache_key_idx <- grep("cacheKeyExpr\\s*=", lines)
      if (length(cache_key_idx) > 0) {
        for (idx in cache_key_idx) {
          # Remove trailing comma from the line before cacheKeyExpr
          if (idx > 1) {
            lines[idx - 1] <- sub(",\\s*$", "", lines[idx - 1])
          }
          # Count closing parens that come after the cacheKeyExpr value.
          # The } closes the cacheKeyExpr value, and ) closes renderPlot.
          # We want to keep only the outermost trailing ) chars.
          line_text <- lines[idx]
          indent <- sub("\\S.*", "", line_text)
          # Extract everything after the last } (which closes the expr value)
          after_last_brace <- sub(".*\\}", "", line_text)
          # Count closing parens in that trailing bit
          n_close <- nchar(gsub("[^)]", "", after_last_brace))
          if (n_close > 0) {
            lines[idx] <- paste0(indent, paste(rep(")", n_close), collapse = ""))
          } else {
            # No closing parens on this line — just comment it out
            lines[idx] <- paste0("# [shinylive-patch] ", line_text)
          }
        }
      }
      file_patches <- c(file_patches, "renderCachedPlot -> renderPlot")
    }

    # ---- 4. File writes: comment out saveRDS, write.csv, write_csv, etc. ----
    write_pattern <- paste0(
      "^(?!\\s*#).*\\b(",
      "saveRDS|save\\.image|write\\.csv|write\\.csv2|write\\.table|",
      "write\\.rds|write_csv|write_csv2|write_tsv|write_rds|",
      "write_lines|writeLines|write_file|write_delim",
      ")\\s*\\("
    )
    write_idx <- grep(write_pattern, lines, perl = TRUE)
    if (length(write_idx) > 0) {
      for (idx in write_idx) {
        lines[idx] <- paste0("# [shinylive-patch] ", lines[idx])
      }
      file_patches <- c(file_patches,
                         paste0("commented out ", length(write_idx), " file-write call(s)"))
    }

    # ---- 5. Remote URL reads: download at build time ----
    # Match read_csv("http..."), read.csv("http..."), read_tsv, read_delim,
    # read.table, read_rds, readRDS with URL arguments
    url_read_pattern <- paste0(
      "(read_csv|read\\.csv|read_csv2|read\\.csv2|read_tsv|read\\.tsv|",
      "read_delim|read\\.delim|read\\.table|read_rds|readRDS|",
      "read_lines|readLines|read_file)\\s*\\(\\s*[\"'](https?://[^\"']+)[\"']"
    )
    url_matches <- gregexpr(url_read_pattern, lines, perl = TRUE)
    for (i in seq_along(lines)) {
      m <- regmatches(lines[i], url_matches[[i]])
      if (length(m) == 0 || all(m == "")) next
      if (grepl("^\\s*#", lines[i])) next  # skip commented lines

      for (match_str in m) {
        # Extract the URL
        url_m <- regmatches(match_str,
                            regexpr("[\"'](https?://[^\"']+)[\"']", match_str))
        url <- gsub("[\"']", "", url_m)

        # Extract the function name
        func <- sub("\\s*\\(.*", "", match_str)

        # Determine file extension from URL
        url_path <- sub("\\?.*$", "", url)
        ext <- tolower(tools::file_ext(url_path))
        if (ext == "") ext <- "csv"  # default to csv

        # Create a safe local filename
        url_slug <- gsub("[^a-zA-Z0-9]", "_", basename(url_path))
        local_name <- paste0("remote_", url_slug)
        if (!grepl(paste0("\\.", ext, "$"), local_name)) {
          local_name <- paste0(local_name, ".", ext)
        }

        # Download the file
        data_dir <- file.path(app_dir, "data")
        if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
        local_path <- file.path(data_dir, local_name)
        rel_path <- file.path("data", local_name)

        tryCatch({
          cli::cli_alert_info("Downloading remote data: {.url {url}}")
          utils::download.file(url, local_path, mode = "wb", quiet = TRUE)
          size <- file.size(local_path)
          cli::cli_alert_success(
            "Saved {.file {rel_path}} ({(.deploy_format_size(size))})"
          )

          # Rewrite the line to use the local path
          lines[i] <- gsub(
            paste0("[\"']", gsub("([\\[\\](){}.*+?^$|\\\\])", "\\\\\\1", url), "[\"']"),
            paste0('"', rel_path, '"'),
            lines[i]
          )
          file_patches <- c(file_patches,
                             paste0("remote URL -> ", rel_path))
        }, error = function(e) {
          cli::cli_alert_warning(
            "Could not download {.url {url}}: {e$message}"
          )
          # Comment out the line since the remote URL won't work anyway
          lines[i] <<- paste0("# [shinylive-patch: download failed] ", lines[i])
          file_patches <<- c(file_patches,
                              paste0("commented out unreachable URL: ", url))
        })
      }
    }

    # ---- Write patched file if changed ----
    if (!identical(lines, original)) {
      writeLines(lines, f)
      patched <- c(patched, f)
      patches_applied[[fname]] <- file_patches
      for (p in file_patches) {
        cli::cli_alert_info("Patched {.file {fname}}: {p}")
      }
    }
  }

  if (length(patched) == 0) {
    cli::cli_alert_info("No Shinylive compatibility patches needed")
  }

  invisible(patched)
}


