#' install_init_pkgs
#' @description Installs the initial recommended packages
#' @param on_exit_restart Logical. Restart R session of install.
#' @export
install_init_pkgs <- function(on_exit_restart = TRUE){

  if(on_exit_restart) on.exit(if (interactive()) rstudioapi::restartSession(TRUE))

  # Ensure pak is installed
  install_pak()

  # Core utilities
  pak::pkg_install(c("magrittr", "devtools", "stringi"))
  library("magrittr")


  # Tidyverse / Modeling / Shiny
  c(
    "rmarkdown", "shiny", "tidymodels", "tidyverse"
  ) %>%
    pak::pkg_install()


  pkgs <- c(
    "abind", "anytime", "askpass", "assertthat",
    "backports", "BiocManager", "brew", "brio",
    "broom", "bslib", "cachem", "callr",
    "car", "carData", "caret", "caTools",
    "cellranger", "checkmate", "chron", "cli",
    "clipr", "coda", "colorspace", "commonmark",
    "conflicted", "crayon", "credentials", "crosstalk",
    "curl", "data.table", "datapasta", "DBI",
    "dbplyr", "desc", "dials", "DiceDesign",
    "dichromat", "diffobj", "digest", "doParallel",
    "dplyr", "drat", "dtplyr", "dygraphs",
    "e1071", "easystats", "effects", "ellipsis",
    "emoji", "encryptr", "evaluate", "extraDistr",
    "fansi", "farver", "fastmap", "fBasics",
    "flexdashboard", "fontawesome", "forcats", "foreach",
    "forecast", "formatR", "fs", "furrr",
    "future", "future.apply", "gargle", "gdata",
    "generics", "gert", "ggforce", "ggfortify",
    "ggmap", "ggplot2", "ggvis", "gh",
    "gitcreds", "glmnet", "globals", "glue",
    "googledrive", "googlesheets4", "googleVis", "gower",
    "GPfit", "gplots", "gridExtra", "gt",
    "gtable", "gtools", "hardhat", "haven",
    "here", "highcharter", "highr", "Hmisc",
    "hms", "htmltools", "htmlwidgets", "httpuv",
    "httr", "ids", "igraph", "infer",
    "ini", "inline", "ipred", "isoband",
    "iterators", "janitor", "jpeg", "jquerylib",
    "jsonlite", "kernlab", "knitr", "labeling",
    "later", "latticeExtra", "lava", "lavaan",
    "lazyeval", "leaflet", "leaps", "lhs",
    "lifecycle", "listenv", "lme4", "lmtest",
    "loo","lubridate",
    "magrittr", "manipulate", "maps",
    "markdown", "matrixcalc", "MatrixModels", "matrixStats",
    "memoise", "mime", "minqa", "mnormt",
    "modeldata", "ModelMetrics", "modelr", "modeltime",
    "modeltools", "multcomp", "mvtnorm", "NLP",
    "nnet", "odbc", "officer", "openssl",
    "parallel", "parallelly", "parsnip", "patchwork",
    "pillar", "plotly", "plotrix", "plumber",
    "plyr", "png", "prettyunits", "pROC",
    "prodlim", "progress", "progressr", "promises",
    "prophet", "proto", "proxy", "ps",
    "psych", "purrr", "quadprog",  "Quandl",
    "quantmod", "quantreg", "randomForest", "rappdirs",
    "rcmdcheck", "Rcmdr", "RColorBrewer", "Rcpp",
    "RcppArmadillo", "RcppEigen", "RcppParallel", "RcppRoll",
    "RCurl", "readr", "readxl", "recipes",
    "relimp", "remotes", "reshape", "reshape2",
    "rgl", "rjson", "RJSONIO",
    "rlang", "RODBC", "roxygen2", "rpart",
    "rsample", "RSQLite", "rstan", "rstantools",
    "rversions", "rvest",
    "sandwich", "sass", "scales", "scatterD3",
    "scatterplot3d", "sem", "shape", "sourcetools",
    "sp", "spatial", "stringi", "stringr",
    "survival", "sys", "testthat", "tibble",
    "tidyquant", "tidyr", "tidyselect", "timeDate",
    "timeSeries", "timetk", "tinytex", "tm",
    "tree", "tseries", "tsfeatures", "tune",
    "tzdb", "urca", "usethis", "utf8",
    "uuid", "vcd", "vctrs", "viridisLite",
    "vroom", "withr", "workflows", "workflowsets",
    "xfun", "xgboost", "XLConnect", "xlsx",
    "xlsxjars", "XML","xml2", "xopen",
    "xtable", "xts", "yaml", "yardstick",
    "zip", "zoo"
  )

  pak_install_by_chunk(pkgs)



  pkgs <- easystats:::.suggested_pkgs() %>%
    unlist() %>%
    unique() %>%
    sort()

  pak_install_by_chunk(pkgs)


  pkgs <- c(
    "argonR", "bs4Dash",
    "bsplus", "cicerone",
    "colourpicker", "drawer",
    "DT", "excelR",
    "faq", "flashCard",
    "formattable", "kableExtra",
    "reactable", "rhandsontable",
    "rhino", "scrollrevealR",
    "semantic.dashboard", "sever",
    "shiny.blueprint", "shiny.fluent",
    "shiny.react", "shiny.router",
    "shiny.worker",  "shinyalert",
    "shinybrowser", "shinybusy",
    "shinyChatR", "shinydashboard",
    "shinydashboardPlus", "shinydisconnect",
    "shinyFeedback", "shinyFiles",
    "shinyglide", "shinyhelper",
    "shinyjs", "shinymaterial",
    "shinyMobile", "shinythemes",
    "shinyTime", "shinyvalidate",
    "shinyWidgets", "slickR",
    "sortable", "spsComps",
    "tablerDash", "timevis",
    "tippy"
  )
  pak_install_by_chunk(pkgs)


  c("google/CausalImpact") %>% pak::pkg_install()


  c(
    "RinteRface/fullPage",
    "rstudio/gridlayout",
    "datasketch/shinypanels"
  ) %>% pak::pkg_install()


  c("fortunes", "rladies/praise", "haukelicht/dadjokes") %>% pak::pkg_install()

}
