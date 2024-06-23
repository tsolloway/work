#' hc_theme_picker
#' @description hc_theme_picker
#' @export
hc_theme_picker <- function(
    theme = c(
      "538", "alone", "bloom", "chalk", "darkunica", "db", "economist",
      "elementary", "ffx", "flat", "flatdark", "ft", "ggplot2", "google",
      "gridlight", "handdrawn", "hcrt", "merge", "monokai", "null",
      "sandsignika", "smpl", "sparkline", "sparkline_vb", "superheroes",
      "tufte","tufte2"
    )
){

  require(highcharter)

  if( !is_nothing(theme) && !isFALSE(theme) )  theme <- match.arg(theme)

  theme <- match.arg(theme)

  theme <- switch(
    theme,
    "538" = hc_theme_538(),
    "alone" = hc_theme_alone(),
    "bloom" = hc_theme_bloom(),
    "chalk" = hc_theme_chalk(),
    "darkunica" = hc_theme_darkunica(),
    "db" = hc_theme_db(),
    "economist" = hc_theme_economist(),
    "elementary" = hc_theme_elementary(),
    "ffx" = hc_theme_ffx(),
    "flat" = hc_theme_flat(),
    "flatdark" = hc_theme_flatdark(),
    "ft" = hc_theme_ft(),
    "ggplot2" = hc_theme_ggplot2(),
    "google" = hc_theme_google(),
    "gridlight" = hc_theme_gridlight(),
    "handdrawn" = hc_theme_handdrawn(),
    "hcrt" = hc_theme_hcrt(),
    "merge" = hc_theme_merge(),
    "monokai" = hc_theme_monokai(),
    "null" = hc_theme_null(),
    "sandsignika" = hc_theme_sandsignika(),
    "smpl" = hc_theme_smpl(),
    "sparkline" = hc_theme_sparkline(),
    "sparkline_vb" = hc_theme_sparkline_vb(),
    "superheroes" = hc_theme_superheroes(),
    "tufte" = hc_theme_tufte(),
    "tufte2" = hc_theme_tufte2()
  )
}
