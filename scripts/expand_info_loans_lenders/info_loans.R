source("scripts/data_loading/data_loading_loans.R")
#loans
get_male_borrowers<-function(x){
  return(length(unlist(str_extract_all(x,' male|^male'))))
}
get_female_borrowers<-function(x){
  return(length(unlist(str_extract_all(x,' female|^female'))))
}

get_pictured_borrowers<-function(x){
  return(length(unlist(str_extract_all(x,'true'))))
}
get_non_pictured_borrowers<-function(x){
  return(length(unlist(str_extract_all(x,'false'))))
}


get_info_group<-function(male,female){
  if(male==0){
    if(female==0){
      return('Nobody')
    } else if (female==1){
      return('Only Woman') #only a woman
    } else {
      return('Group Women') #more than a woman, no man
    }
  } else {
    if(female==0){
      if (male==1){
        return('Only Man') #only a man
      } else {
        return('Group Men') #more than a man, no woman
      }
    } else { #there are man and woman
      if(female>male){
        return('Mixed, Women') #group with more females than males
      } else {
        return('Mixed, Men') #group with more males than females
      }
    }
  }
}

loans$male_borrowers_num<-mapply(loans$borrower_genders,  FUN=get_male_borrowers)
loans$female_borrowers_num<-mapply(loans$borrower_genders,  FUN=get_female_borrowers)
loans$pictured_borrowers_num<-mapply(loans$borrower_pictured,  FUN=get_pictured_borrowers)
loans$non_pictured_borrowers_num<-mapply(loans$borrower_pictured,  FUN=get_non_pictured_borrowers)
loans$has_image<-ifelse(is.na(loans$image_id),0,1)

save(loans,
     file = "/Users/marcvalenti/TFM_data/loans_info_expanded.RData")