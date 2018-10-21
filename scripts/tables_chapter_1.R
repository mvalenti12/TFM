
number_loans <- nrow(loans)
number_lenders <- nrow(lenders)

write_to_txt(number_loans, file = "chapter1.txt")
write_to_txt(number_lenders, file = "chapter1.txt")

number_languages <- length(unique(loans$original_language))
print_latex_table(x = loans$original_language,
                  table_header = "Language",
                  table_title = "Number of Loans by Language of Description",
                  table_reference = "Loan_Language",
                  n_digits = 1,
                  number = 4)

number_sectors <- length(unique(loans$sector_name))
print_latex_table(x = loans$sector_name,
                  table_header = "Sector",
                  table_title = "Number of Loans by Sector",
                  table_reference = "Loan_Sector",
                  n_digits = 1,
                  number = 20)

number_countries <- length(unique(loans$country_name))
print_latex_table(x = loans$country_name,
                  table_header = "Country",
                  table_title = "Number of Loans by Country",
                  table_reference = "Loan_Country",
                  n_digits = 1,
                  number = 10)

print_latex_table(x = loans$repayment_interval,
                  table_header = "Repayment Interval",
                  table_title = "Number of Loans by Repayment Interval",
                  table_reference = "Loan_Repayment_Interval",
                  n_digits = 1,
                  number = 5)

print_latex_table(x = lenders$country_name,
                  table_header = "Country",
                  table_title = "Number of Lenders by Country",
                  table_reference = "Lenders_Country",
                  n_digits = 1,
                  number = 5)
