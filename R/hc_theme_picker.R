#' Pick a Highcharter theme
#'
#' @description
#' Selects a Highcharter theme by name and returns the corresponding theme object.
#'
#' @param theme Character; one of the following themes:
#' "538", "alone", "bloom", "chalk", "darkunica", "db", "economist",
#' "elementary", "ffx", "flat", "flatdark", "ft", "ggplot2", "google",
#' "gridlight", "handdrawn", "hcrt", "merge", "monokai", "null",
#' "sandsignika", "smpl", "sparkline", "sparkline_vb", "superheroes",
#' "tufte", "tufte2". Defaults to `"null"`.
#'
#' @return A Highcharter theme object.
#'
#' @examples
#' \dontrun{
#' hc_theme_picker("ggplot2")
#' hc_theme_picker("darkunica")
#' }
#'
#' @export
hc_theme_picker <- function(
    theme = c(
      "538", "alone", "bloom", "chalk", "darkunica", "db", "economist",
      "elementary", "ffx", "flat", "flatdark", "ft", "ggplot2", "google",
      "gridlight", "handdrawn", "hcrt", "merge", "monokai", "null",
      "sandsignika", "smpl", "sparkline", "sparkline_vb", "superheroes",
      "tufte","tufte2"
    )
) {

  if (!requireNamespace("highcharter", quietly = TRUE)) {
    stop("Package 'highcharter' is required for hc_theme_picker()")
  }

  theme <- match.arg(theme)

  switch(
    theme,
    "538" = highcharter::hc_theme_538(),
    "alone" = highcharter::hc_theme_alone(),
    "bloom" = highcharter::hc_theme_bloom(),
    "chalk" = highcharter::hc_theme_chalk(),
    "darkunica" = highcharter::hc_theme_darkunica(),
    "db" = highcharter::hc_theme_db(),
    "economist" = highcharter::hc_theme_economist(),
    "elementary" = highcharter::hc_theme_elementary(),
    "ffx" = highcharter::hc_theme_ffx(),
    "flat" = highcharter::hc_theme_flat(),
    "flatdark" = highcharter::hc_theme_flatdark(),
    "ft" = highcharter::hc_theme_ft(),
    "ggplot2" = highcharter::hc_theme_ggplot2(),
    "google" = highcharter::hc_theme_google(),
    "gridlight" = highcharter::hc_theme_gridlight(),
    "handdrawn" = highcharter::hc_theme_handdrawn(),
    "hcrt" = highcharter::hc_theme_hcrt(),
    "merge" = highcharter::hc_theme_merge(),
    "monokai" = highcharter::hc_theme_monokai(),
    "null" = highcharter::hc_theme_null(),
    "sandsignika" = highcharter::hc_theme_sandsignika(),
    "smpl" = highcharter::hc_theme_smpl(),
    "sparkline" = highcharter::hc_theme_sparkline(),
    "sparkline_vb" = highcharter::hc_theme_sparkline_vb(),
    "superheroes" = highcharter::hc_theme_superheroes(),
    "tufte" = highcharter::hc_theme_tufte(),
    "tufte2" = highcharter::hc_theme_tufte2()
  )
}
