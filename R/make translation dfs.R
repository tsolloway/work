# setwd("~/Library/CloudStorage/OneDrive-WilshireLawFirm/monthly-reports/2024-01/dmd-2024-01")
#
#
# df_names <- readxl::read_excel("~/Library/CloudStorage/OneDrive-WilshireLawFirm/monthly-reports/2024-01/dmd-2024-01/DMD & Settlement-old.xlsx", sheet = "names") %>% tibble::as_tibble()
# df_limit <- readxl::read_excel("~/Library/CloudStorage/OneDrive-WilshireLawFirm/monthly-reports/2024-01/dmd-2024-01/DMD & Settlement-old.xlsx", sheet = "limits") %>% tibble::as_tibble()
#
# names(df_dmd) <- names(df_dmd) %>% work::str_scrub(make_lowercase = TRUE)
# names(df_names) <- names(df_names) %>% work::str_scrub(make_lowercase = TRUE)
# names(df_limit) <- names(df_limit) %>% work::str_scrub(make_lowercase = TRUE)
#
# df_limit <- df_limit[, c("input", "translation", "valid_limits")]
#
#
# df_translate_limtis <- df_limit
# df_translate_employee_names <- df_names
#
# setwd("/Users/tylersolloway/Documents/GitHub/work")
# usethis::use_data(df_translate_limtis, overwrite = T)
# usethis::use_data(df_translate_employee_names, overwrite = T)
#
#
# work::restart()
#
# work::install_pkg_local("work")
#
