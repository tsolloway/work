


work::start(T)

df <- work::lit_get_lead_gen_source()


df$lead_gen_source[is.na(df$match)]


df %>% filter(is.na(match))

df %>% names

lead_gen_backcast <- df

translate_lead_gen <- work::df_translate_lead_gen

translate_lead_gen %>% write("downloads")

lead_gen_backcast %>% write("downloads")


translate_employee_names <- work::df_translate_employee_names


translate_employee_names %>% write("downloads")

df_translate_lead_gen[177, "source_original"] <- "PI Boost English"
df_translate_lead_gen[370, "source_original"] <- "WLF - Nathan Kingery"
df_translate_lead_gen[370, ]

df_translate_lead_gen[738,]


usethis::use_data(df_translate_lead_gen, overwrite = T)
df_translate_lead_gen



