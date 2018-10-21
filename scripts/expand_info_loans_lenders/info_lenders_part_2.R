source("scripts/libraries_functions.R")
load("/Users/marcvalenti/TFM_data/lenders_info_expanded.RData")
# converts the lenders_info_expanded to completely hot_variable_encoding so it is easier to deal in the future.

#we want to include the top 30 lenders countries
aux_countries<-as.data.frame(table(lenders_info_expanded$country_code)) %>%
  arrange(-Freq) %>%
  top_n(wt=Freq,20) %>%
  select(Var1) %>%
  set_colnames(c("country_name")) %>%
  arrange(country_name) %>%
  as.data.table(key="country_name")

# convertir a "no es top 30 els que no son top 30!!!! 
lenders_info_expanded$country_code <- ifelse(is.na(lenders_info_expanded$country_code),
                                             NA,
                                             ifelse(lenders_info_expanded$country_code%in%unlist(aux_countries),
                                                    lenders_info_expanded$country_code,
                                                    "OTHER"))
lenders_info_expanded$member_since <- as.factor(lenders_info_expanded$member_since)
lenders_info_expanded$gender <- as.factor(lenders_info_expanded$gender)
lenders_info_expanded$has_image <- as.factor(ifelse(is.na(lenders_info_expanded$image_id),"no","yes"))



variables_to_transform <- c("member_since","gender","country_code","has_image")

dmy <- dummyVars(" ~ .", 
                 data = lenders_info_expanded[,variables_to_transform],
                 fullRank = FALSE)

lenders_hot_encoded <- data.frame(predict(dmy,
                                          newdata = lenders_info_expanded[,variables_to_transform]))

lenders_info_expanded_hot <- cbind(lenders_info_expanded,
                                   lenders_hot_encoded) %>%
  select(id,
         colnames(lenders_hot_encoded),
         starts_with("occ_")) %>%
  as.data.table(key="id")

save(lenders_info_expanded_hot,
     file = "/Users/marcvalenti/TFM_data/lenders_info_expanded_hot.RData")
                                  