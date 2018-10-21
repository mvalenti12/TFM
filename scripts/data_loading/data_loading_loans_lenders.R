source("scripts/libraries_functions.R")

loans_lenders<-read_csv("/Users/marcvalenti/TFM_data/kiva_ds_csv/loans_lenders.csv")
colnames(loans_lenders) <- unlist(lapply(names(loans_lenders), function(x) str_to_lower(x)))

