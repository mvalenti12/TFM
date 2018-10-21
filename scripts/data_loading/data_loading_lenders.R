source("scripts/libraries_functions.R")

lenders<-read_csv("/Users/marcvalenti/TFM_data/kiva_ds_csv/lenders.csv",col_names=TRUE)

aux_country_codes_lenders <- fromJSON("https://pkgstore.datahub.io/core/country-list/data_json/data/8c458f2d15d9f2119654b29ede6e45b8/data_json.json") %>%
  set_colnames(c("country_code","country_name")) %>%
  mutate(country_name_long = paste0(country_name,
                                    " (",
                                    country_code,
                                    ")"))
lenders <- lenders %>%
  set_colnames(c(c("id","name","image_id","city","state","country_code","member_since","personal_url","occupation","loan_because","other_info","loan_count","invited_by","invited_count"))) %>%
  mutate(aux_country_codes =  as.factor(ifelse(country_code=="NA",NULL,country_code)),
         member_since = to_date(member_since)) %>%
  left_join(aux_country_codes_lenders, by = "country_code")
