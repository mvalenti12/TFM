source("scripts/libraries_functions.R")
source("scripts/data_loading.R")

median_loan <- median(loans$loan_amount)

write_to_txt(median_loan,
             file = "chapter1.txt")
lenders_joiners_2017 <- sum(year(to_date(lenders$MEMBER_SINCE))==2017)
loans_created_2017 <- sum(year(loans$posted_time)==2017)
loans_raised_2017 <- paste0(round(sum(loans[year(loans$posted_time)==2017,'funded_amount'])/1000000,2),"M")
number_loans_funded <- sum((loans$funded)==TRUE)
funding_rate <- round(sum((loans$funded)==TRUE)/nrow(loans)*100,2)

write_to_txt(lenders_joiners_2017,
             file = "chapter1.txt")

write_to_txt(loans_created_2017,
             file = "chapter1.txt")

write_to_txt(loans_raised_2017,
             file = "chapter1.txt")

write_to_txt(number_loans_funded,
             file = "chapter1.txt")

write_to_txt(funding_rate,
             file = "chapter1.txt")
