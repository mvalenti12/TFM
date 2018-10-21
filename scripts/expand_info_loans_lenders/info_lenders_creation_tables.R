load("/Users/marcvalenti/TFM_data/lenders_info_expanded.RData")
source("/Users/marcvalenti/TFM/scripts/libraries_functions.R")
occupations <- colSums(lenders_info_expanded[,startsWith(colnames(lenders_info_expanded),"occ_")],na.rm=TRUE) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  mutate(occupation = str_replace_all(rowname,"occ_","")) %>%
  select(-rowname) %>%
  set_colnames(c("lenders","occupation")) %>%
  arrange(occupation)

occupations$matches <- c("musician|writer|book",
                         "consultant|accountant|business|sales|marketing|banker|finance|ceo|analyst|director|administrator|econom",
                         "teacher|professor|educat",
                         "entrepreneur",
                         "nurse|physician|doctor|dentist|psychologist|pharmacist|medi",
                         "developer|programmer|it",
                         "law|attorney",
                         "retired",
                         "student")

occupations <- occupations %>%
  select(occupation,
         matches,
         lenders) %>%
  as.data.frame()

occupations <- c("student",
              "teacher",
              "professor",
              "educat",
              "retired",
              "entrepreneur",
              "consultant",
              "accountant",
              "business",
              "sales",
              "marketing",
              "banker",
              "finance",
              "ceo",
              "analyst",
              "director",
              "administrator",
              "econom",
              "law",
              "attorney",
              "nurse",
              "physician",
              "doctor",
              "dentist",
              "psychologist",
              "pharmacist",
              "medi",
              "developer",
              "programmer",
              "it",
              "musician",
              "writer",
              "book") %>%
  as.data.frame() %>%
  set_colnames("occ_name") %>%
  mutate(count = NA,
         pattern_seeked = as.character(occ_name),
         category = c("student",
                      rep("education",3),
                      "retired",
                      "entrepreneur",
                      rep("business",12),
                      rep("law",2),
                      rep("health",7),
                      rep("it",3),
                      rep("arts",3)))

for (i in 1:nrow(occupations)){
  
  occupations$count[i] <- sum(str_count(tolower(lenders_info_expanded$occupation),
                                 pattern = occupations$pattern_seeked[i]),
                           na.rm = TRUE)
  }
sum(occupations$count)/nrow(lenders_info_expanded)
sum(!is.na(lenders_info_expanded$occupation))/nrow(lenders_info_expanded)
occupations <- occupations %>%
  group_by(category) %>%
  mutate(group_count = sum(count),
         group_count_perc = paste0(round(sum(count)/nrow(lenders_info_expanded)*100,2),"%")) %>%
  select(category,
         pattern_seeked,
         count,
         group_count,
         group_count_perc) %>%
  arrange(category,
          pattern_seeked)
occupations[nrow(occupations)+1,] <- c("Other",
                                       "Non NULL",
                                       sum(!is.na(lenders_info_expanded$occupation))-sum(occupations$count),
                                       sum(!is.na(lenders_info_expanded$occupation))-sum(occupations$count),
                                       paste0(round((sum(!is.na(lenders_info_expanded$occupation))-sum(occupations$count))/nrow(lenders_info_expanded)*100,2),"%"))
occupations[nrow(occupations)+1,] <- c("Null Description",
                                       "NULL",
                                       nrow(lenders_info_expanded)-sum(as.numeric(occupations$count)),
                                       nrow(lenders_info_expanded)-sum(as.numeric(occupations$count)),
                                       paste0(round((nrow(lenders_info_expanded)-sum(as.numeric(occupations$count)))/nrow(lenders_info_expanded)*100,2),"%"))

# the file "/Users/marcvalenti/TFM/Tables/table_Lenders_Occupation.tex" was later edited manually.
# generating the code gives a start to that file though.
# print(
#   xtable(occupations, 
#          type = "latex", 
#          caption = "(Summarized) Occupations of Lenders ", 
#          label = "tab:lenders_occupation"),
#   include.rownames=FALSE,
#   file = "/Users/marcvalenti/TFM/Tables/table_Lenders_Occupation.tex"
# )
n_lenders_occupation_null <- unlist(c(occupations[occupations$category=="Null Description","group_count_perc"]))
n_lenders_occupation_no_matched <- unlist(c(occupations[occupations$category=="Other","group_count_perc"]))
n_lenders_occupation_yes_matched <- occupations %>%
  ungroup() %>%
  filter(!category%in%c("Null Description","Other")) %>%
  summarise(sum = paste0(round(sum(as.numeric(count))/nrow(lenders_info_expanded)*100,2),"%")) %>%
  unlist()

write_to_txt(n_lenders_occupation_null,
             file = "chapter1.txt")

write_to_txt(n_lenders_occupation_no_matched,
             file = "chapter1.txt")

write_to_txt(n_lenders_occupation_yes_matched,
             file = "chapter1.txt")
