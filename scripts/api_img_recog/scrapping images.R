# https://stackoverflow.com/questions/28852057/change-ip-address-dynamically
# https://tor.stackexchange.com/questions/2006/how-to-run-multiple-tor-browsers-with-different-ips
# source("scripts/data_loading/data_loading_loans.R")
# 
# dt <- loans %>%
#          # Country is Philippines
#   filter(country_name=="Philippines", 
#          # Partner Id is 145
#          partner_id==145) %>%         
#         # Posted Date is after 2016-03-01
#   filter(to_date(posted_time)>='2016-03-01', 
#          # Posted Date is before 2016-04-01
#          to_date(posted_time)<'2016-04-01') %>% 
#   # There is only ONE borrower; and is FEMALE
#   filter(borrower_genders=="female",  
#          # There is only ONE borrower IN THE PICTURE
#          borrower_pictured=="true",   
#          # The repayment interval is irregular
#          repayment_interval=="irregular", 
#          # The distribution model is through field partner
#          distribution_model=="field_partner",
#          # The sector Name is either Agriculture, Food or Retail
#          sector_name%in%c("Agriculture","Food","Retail")) 
# save(dt,file = "/Users/marcvalenti/TFM_data/dt_loans_reduced_philippines.RData")
# rm(loans)
#gets a reduction of 2.7k pictures
load("/Users/marcvalenti/TFM_data/dt_loans_reduced_philippines.RData")
source("/Users/marcvalenti/TFM/scripts/api_img_recog/get_img_link_attr_fun.R")
load("/Users/marcvalenti/TFM_data/image_recognition.RData")
# res_google <- matrix(NA,
#               ncol = 9,
#               nrow = nrow(dt))
# rownames(res_google) <- dt$loan_id
# res_azure <- matrix(NA,
#                      ncol = 8,
#                      nrow = nrow(dt))
# rownames(res_azure) <- dt$loan_id

for (i in 1:nrow(dt)){
    print(i)  
  link <- get_image_link(dt$loan_id[i])
  if(!is.na(link)){
    res_azure[i,] <- get_azure_response(link)
    res_google[i,] <- get_google_response(link)
    Sys.sleep(20)
  }
}

# save(res_azure,
# res_google,
# i,
# file="/Users/marcvalenti/TFM_data/image_recognition.RData")
