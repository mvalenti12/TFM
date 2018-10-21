source("scripts/libraries_functions.R")

loans<-read_csv("/Users/marcvalenti/TFM_data/kiva_ds_csv/loans.csv") #was not able to read the JSON
colnames(loans) <- unlist(lapply(names(loans), function(x) str_to_lower(x)))
loans$funded<-loans$funded_amount==loans$loan_amount