source("scripts/data_loading/data_loading_lenders.R")
source("scripts/libraries_functions.R")
# incorporates information about gender, occupation and image.

#lenders
#get some names metadata
metadata_names<-fread("https://raw.githubusercontent.com/hadley/data-baby-names/master/baby-names.csv")
metadata_names<-metadata_names %>%
  mutate(name=tolower(name)) %>%
  group_by(name) %>%
  mutate(local_max=max(percent)) %>%
  filter(local_max==percent) %>%
  ungroup() %>%
  mutate(gender=ifelse(sex=="boy","male","female")) %>%
  select(name,gender) 

predict_gender <- function(x){
  # initiate the counter of male to 0
  male <- 0
  # initiate the counter of female to 0
  female <- 0
  # split the string into different ones.
  
  # this is done to identify couples that are registered with both a female and male name
  # e.g "Jose y Anna"
  splitted <- unlist(str_split(x,"-|\n| "))
  # for every splitted string
  for (i in 1:length(splitted)){
    # if there is a match with any of the male names in the dictionary, increase the male counter
    if(splitted[i]%in%unlist(metadata_names[metadata_names$gender=="male",'name'])){
      male <- male + 1
      # if there is a match with any of the male fenames in the dictionary, increase the female counter
    } else if (splitted[i]%in%unlist(metadata_names[metadata_names$gender=="female",'name'])){
      female <- female + 1
    }
  }
  # return the result
  return(ifelse(male*female>=1,
                "couple",
                ifelse(male>=1,
                       "male",
                       ifelse(female>=1,
                              "female","NA"))))
}



#take all
lenders_gender_pred<-lenders %>%
  select(id,name,image_id,occupation,country_code,loan_count,member_since) %>%
  mutate(name = tolower(name),
         member_since = year(member_since),
         occupation = tolower(occupation),
         has_image = !is.na(image_id)) %>%
  left_join(metadata_names,by="name") %>%
  mutate(occ_student = str_count(occupation, "student"),
         occ_education = str_count(occupation, "teacher|professor|educat"),
         occ_retired = str_count(occupation, "retired"),
         occ_entrepreneur = str_count(occupation, "entrepreneur"),
         occ_business = str_count(occupation, "consultant|accountant|business|sales|marketing|banker|finance|ceo|analyst|director|administrator|econom"),
         occ_law = str_count(occupation, "law|attorney"),
         occ_health = str_count(occupation, "nurse|physician|doctor|dentist|psychologist|pharmacist|medi"),
         occ_it = str_count(occupation, "developer|programmer|it"),
         occ_arts = str_count(occupation, "musician|writer|book")) %>%
  as.data.table(key="id")

exact_match <- lenders_gender_pred %>%
    group_by(gender) %>%
    summarise(n=n()) %>%
    mutate(gender = factor(ifelse(is.na(gender),"NA",gender)))
  
lenders_no_gender <- lenders_gender_pred %>%
  filter(is.na(gender))
# takes some time to run ~3 hours
lenders_no_gender$gender <- mapply(lenders_no_gender$name, FUN = predict_gender)

lenders_yes_gender <- lenders_gender_pred %>%
  filter(!is.na(gender))

lenders_info_expanded <- rbind(lenders_yes_gender,lenders_no_gender)

predictions_match_merged <- lenders_info_expanded %>%
  group_by(gender) %>%
  summarise(n=n())

lenders_info_expanded <- lenders_info_expanded %>%
  inner_join(lenders[,c("id","member_since")]) %>%
  mutate(member_since = year(member_since))

# uncomment if the file wants to be saved, but the current one works.
# save(lenders_info_expanded,
#      file = "/Users/marcvalenti/TFM_data/lenders_info_expanded.RData")

# generates the latex table
# save(exact_match,predictions,predictions_match_merged, file = "/Users/marcvalenti/TFM_data/lenders_gender_res.RData")

res_table <- final_res %>%
  set_colnames(c("gender","n")) %>%
  left_join(exact_match,by = "gender") %>%
  column_to_rownames(var = "gender") %>%
  set_colnames(c("TOTAL","exact_match")) %>%
  t()
res_table[is.na(res_table)] <- 0
res_table <- rbind(res_table,
                   res_table[1,] - res_table[2,])
res_table <- res_table[c(2,3,1),]
rownames(res_table) <- c("exact match", "predict gender","TOTAL")
res_table <- res_table[,-1] 
rS <- rowSums(res_table)
res_table <- cbind(res_table,
                   c(paste0(round(res_table[,1]/rS*100,2),"%")),
                   c(paste0(round(res_table[,2]/rS*100,2),"%")),
                   c(paste0(round(res_table[,3]/rS*100,2),"%")))
colnames(res_table) <- c("couple",
                         "female",
                         "male",
                         "couple (%)",
                         "female (%)",
                         "male (%)")
print(xtable(res_table, 
             type = "latex", 
             align = "lllllll",
             caption = "Enriching Lenders' Gender: Results", 
             label = paste0("tab:lenders_gender")), 
      file = paste0("/Users/marcvalenti/TFM/Tables/table_lenders_gender.tex"),
      include.rownames=TRUE)
lenders_exact_reach <- paste0(round(sum(as.numeric(res_table[1,1:3]))/nrow(lenders_info_expanded)*100,2))
lenders_gender_reach <- paste0(round(sum(as.numeric(res_table[3,1:3]))/nrow(lenders_info_expanded)*100,2))
write_to_txt(lenders_exact_reach,
             file = "chapter1.txt")
write_to_txt(lenders_gender_reach,
             file = "chapter1.txt")
