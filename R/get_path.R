#' get_path
#'
#' @description Returns R system utility paths, optionally opening for editing.
#'
#' @param type Character; path type to return. Choices: "home", "r_home", "environment", "profile",
#'             "makevars", "preference_rstudio", "snippets_r", "git", "onedrive", "downloads", "desktop".
#' @param edit_file Logical; if TRUE, opens the file in RStudio editor (interactive sessions only).
#' @param scope Character; "user" or "project" scope for environment/profile files.
#'
#' @return Character string with the path.
#'
#' @examples
#' \dontrun{
#' get_path("home")
#' get_path("profile", edit_file = TRUE)
#' get_path("downloads")
#' }
#'
#' @export
get_path <- function(
    type = c(
      "home", "r_home", "environment", "profile", "makevars",
      "preference_rstudio", "snippets_r", "git",
      "onedrive", "downloads", "desktop"
    ),
    edit_file = FALSE,
    scope = c("user", "project")
){
  type <- match.arg(type)
  scope <- match.arg(scope)

  path <- switch(
    type,
    home = Sys.getenv("HOME"),
    r_home = Sys.getenv("R_HOME") %||% R.home(),
    environment = usethis::r_env_path(scope),
    profile = usethis::r_profile_path(scope),
    makevars = file.path(Sys.getenv("HOME"), ".R", "Makevars"),
    preference_rstudio = file.path(usethis::rstudio_config_dir(), "rstudio-prefs.json"),
    snippets_r = file.path(usethis::rstudio_config_dir(), "snippets", "r.snippets"),
    git = Sys.getenv("path_git_directory"),
    onedrive = Sys.getenv("OneDrive") %||% stop("OneDrive path not set."),
    downloads = if (get_os() == "windows") file.path(Sys.getenv("USERPROFILE"), "Downloads") else "~/Downloads",
    desktop = if (get_os() == "windows") file.path(Sys.getenv("OneDrive") %||% Sys.getenv("USERPROFILE"), "Desktop") else "~/Desktop"
  )

  # Ensure file exists for editable types
  if (type %in% c("environment", "profile", "preference_rstudio", "snippets_r") && !file.exists(path)) {
    file.create(path)
  }

  # Validate git path
  if (type == "git" && (!is_truthy(path) || !dir.exists(path))) {
    stop("Git path not set or invalid. Use `work::set_r_environment('git_local_dir')`")
  }

  path <- normalizePath(path, mustWork = FALSE, winslash = "/")

  if (edit_file && interactive() && !type %in% c("home", "r_home")) {
    usethis::edit_file(path)
  }

  path
}
