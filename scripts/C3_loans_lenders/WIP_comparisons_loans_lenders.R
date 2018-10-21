

#dt_complete_v2<-cbind(loans_lenders_bigbrother$loan_id,dt_complete[1:k,colSums(dt_complete[1:k,],na.rm=TRUE)!=0]) %>%
# as.data.table()
#data.table::fwrite(dt_complete_v2,file="dt_complete.csv")

load("/Users/marcvalenti/TFM_data/loans_info_expanded.RData")
load("/Users/marcvalenti/TFM_data/loans_by_lenders250000.RData")
source("scripts/libraries_functions.R")

dt_complete <- dt_complete_250000
rm(dt_complete_250000)
aux_country_codes_borrowers <- loans %>%
  filter(!is.na(country_code)) %>%
  distinct(country_code,country_name) %>%
  mutate(country_name_long = paste0(country_name,
                                    " (",
                                    country_code,
                                    ")"))

aux_country_codes_lenders <- fromJSON("https://pkgstore.datahub.io/core/country-list/data_json/data/8c458f2d15d9f2119654b29ede6e45b8/data_json.json") %>%
  set_colnames(c("country_code","country_name")) %>%
  mutate(country_name_long = paste0(country_name,
                                    " (",
                                    country_code,
                                    ")"))


generate_contingency_tables_plots <- function(x,
                                              file_name,
                                              lenders_variable,
                                              loans_variable){
  #generates the common title
  title_fun <- paste0(lenders_variable, " (Lenders) ~ ",
                      loans_variable, " (Borrowers)")
  #generates the mosaic plot
  png(file = paste0("/Users/marcvalenti/TFM/Figures/Contingency/Mosaic_",file_name,".png"))
  mosaicplot( t(x), 
              xlab=paste0("Borrowers (",loans_variable,")"),
              ylab=paste0("Lenders (",lenders_variable,")"),
              shade = TRUE,
              las = 2,
              #cex.axis = 1, 
              main = paste0("Mosaic Diagram for ",title_fun))
  dev.off()
  print(paste0("Mosaic Diagram for ",title_fun))
  
  #generates CA biplot
  res.ca <- CA(x, graph = FALSE)
  p <- fviz_ca_biplot(res.ca, 
                      repel = TRUE,
                      title = paste0("CA - Biplot for ",title_fun)) +
    theme_economist_white()
  ggsave(plot = p,
         filename = paste0("/Users/marcvalenti/TFM/Figures/Contingency/CA_",file_name,".png"))
  print(paste0("MCA - Biplot for ",title_fun))
  
  #generates latex table
  x <- rbind(x,
             as.vector(apply(x, 2, sum)))
  rownames(x)[nrow(x)] <- "TOTAL"
  options(xtable.table.placement="!htb")
  print(xtable(x, 
               type = "latex", 
               caption = paste0("Contingency Table for ",title_fun), 
               label = paste0("tab:contingency_",file_name),
               digits = 0), 
        file = paste0("/Users/marcvalenti/TFM/Tables/Contingency_Tables/table_contingency_",
                      str_replace_all(file_name," ","_"),
                      ".tex"),
        include.rownames=TRUE)
  print(paste0("Contingency Table for ",title_fun))
  
}
loan_id <- rownames(dt_complete)
dt_complete <- dt_complete %>%
  as.data.frame() %>%
  mutate(id = as.integer(loan_id)) %>%
  as.data.table(key = "id")

##on gender
dt_gender<-dt_complete %>%
  select(id,starts_with("gender")) %>%
  left_join(loans,by=c("id"="loan_id")) %>%
  select(starts_with("gender"),male_borrowers_num,female_borrowers_num) %>%
  mutate(info_lenders_female = gender.female/(gender.female+gender.male),
         info_borrowers_female = female_borrowers_num/(female_borrowers_num+male_borrowers_num),
         info_lenders_female_cut = cut(info_lenders_female, breaks = seq(from = 0,to = 1, by = 0.1), include.lowest = TRUE),
         info_borrowers_female_cut = cut(info_borrowers_female, breaks = seq(from = 0,to = 1, by = 0.1), include.lowest = TRUE)) %>%
  filter((male_borrowers_num==0&female_borrowers_num==1)|(male_borrowers_num==1&female_borrowers_num==0))

# num_lenders_fem <- sum(dt_gender$info_lenders_female==1,na.rm=TRUE)
# num_lenders_male <- sum(dt_gender$info_lenders_female==0,na.rm=TRUE)
# num_borrowers_fem <- sum(dt_gender$info_borrowers_female==1,na.rm=TRUE)
# num_borrowers_male <- sum(dt_gender$info_borrowers_female==0,na.rm=TRUE)
# 
# write_to_txt(num_lenders_fem,file="auxiliary_values.txt")
# write_to_txt(num_lenders_male,file="auxiliary_values.txt")
# write_to_txt(num_borrowers_fem,file="auxiliary_values.txt")
# write_to_txt(num_borrowers_male,file="auxiliary_values.txt")

# once is enough 
# prop_female_borrowers <- dt_gender$info_borrowers_female
# prop_female_lenders <- dt_gender$info_lenders_female
# hist_top <- ggplot(dt_gender)+geom_histogram(aes(x = prop_female_lenders)) + labs(x ="Proportion of Female Lenders")
# empty <- ggplot()+geom_point(aes(1,1), colour="white")+
#   theme(axis.ticks=element_blank(),
#         panel.background=element_blank(),
#         axis.text.x=element_blank(), axis.text.y=element_blank(),
#         axis.title.x=element_blank(), axis.title.y=element_blank())
# scatter <- ggplot()+geom_point(aes(x = prop_female_lenders, y = prop_female_borrowers)) + 
#   labs(x ="Proportion of Female Lenders",
#        y = "Proportion of Female Borrowers")
# hist_right <- ggplot()+geom_histogram(aes(x = prop_female_borrowers))+ labs(x = "Proportion of Female Borrowers")+coord_flip() 
# 
# p <- grid.arrange(hist_top, 
#                   empty, 
#                   scatter, 
#                   hist_right, 
#                   ncol=2, 
#                   nrow=2, 
#                   widths=c(4, 1), 
#                   heights=c(1, 4),
#                   top = "Borrower-Lender Relationship: Gender")
# ggsave(p,file = "/Users/marcvalenti/TFM/Figures/Contingency/Gender.png")

# lm_res <- lm(info_borrowers_female~info_lenders_female, data = dt_gender)
# summary(lm_res)
# plot(lm_res)
# https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1


formula <- female_borrowers_num ~ gender.male + gender.female 
formula_log_linear <- log(info_lenders_female)~info_borrowers_female
formula_log_log <- log(info_lenders_female)~log(info_borrowers_female)
#logistic
logistic_res <- glm(formula, 
               data = dt_gender,
               family = "binomial")

summary(logistic_res)
plot(logistic_res)

#quasibinomial
quasibinomial_res <- glm(formula, 
                    data = dt_gender,
                    family = "quasibinomial")
summary(quasibinomial_res)

# nnet::multinom(info_borrowers_female~info_lenders_female, 
#                data = dt_gender)
#linear
linear_res <- lm(formula,
             data = dt_gender)
summary(linear_res)
plot(linear_res)

#log linear
log_linear_res <- lm(formula_log_linear,
                 data = dt_gender)
summary(log_linear_res)
plot(log_linear_res)

#log log
log_log_linear_res <- lm(formula_log_log,
                     data = dt_gender)
summary(log_log_linear_res)
plot(log_log_linear_res)

log(dt_gender$info_borrowers_female)


glm_res <- glm(info_borrowers_female~info_lenders_female, 
               data = dt_gender,
               family = "quasibinomial")
# dt_gender$info_lenders<-mapply(FUN=get_info_group,dt_gender$male,dt_gender$female)
# dt_gender$info_borrowers<-mapply(FUN=get_info_group,dt_gender$male_borrowers_num,dt_gender$female_borrowers_num)

gender_table<-table(dt_gender$info_lenders_female_cut,dt_gender$info_borrowers_female_cut)

generate_contingency_tables_plots(x = gender_table,
                                  file_name = "Gender",
                                  lenders_variable = "Gender",
                                  loans_variable = "Gender")

# expected <- (t(as.vector(colSums(x))*matrix(1,nrow(x),nrow(x)))*as.vector(rowSums(x)))/sum(x)
# raw_diff <- (x - expected)
# 
# 
# # 2. Graph
# balloonplot(t(gender_table), main ="housetasks", xlab ="Proportion of Female Borrowers", ylab="Proportion of Male Borrowers",
#             label = FALSE, show.margins = FALSE)
# 
# chisq.test(dt_gender$info_lenders_female_cut,dt_gender$info_borrowers_female_cut)


##on country
dt_country<-dt_complete %>%
  select(id,starts_with("country")) %>%
  left_join(loans[,c("loan_id",'country_code')],by=c("id"="loan_id")) %>%
  select(-id) %>%
  as.data.frame() %>%
  melt(by="country_code") %>%
  group_by(country_code,variable) %>%
  summarise(value=sum(value,na.rm=TRUE)) %>%
  filter(value!=0) %>%
  set_colnames(c("borrower","lender","value")) %>%
  dcast(lender~borrower,fun.aggregate=sum)

rownames(dt_country) <- unlist(lapply(dt_country$lender, function(x) str_replace_all(x,"country_code","")))

dt_country$lender<-NULL
sort(rowSums(dt_country),decreasing=TRUE)
sort(colSums(dt_country),decreasing=TRUE)

countries_lenders <- 9
countries_borrowers <- 9
top_n_countries_lenders <- names(sort(rowSums(dt_country),decreasing=TRUE))[1:countries_lenders]
top_n_countries_borrowers <- names(sort(colSums(dt_country),decreasing=TRUE))[1:countries_borrowers]


colnames(dt_country) <- ifelse(colnames(dt_country)%in%top_n_countries_lenders,colnames(dt_country),"Other") %>%
  as.data.frame() %>%
  set_colnames(c("country_code")) %>%
  mutate(country_code = ifelse(country_code%in%top_n_countries_lenders,country_code,"Other")) %>%
  left_join(aux_country_codes_borrowers) %>%
  select(country_name_long) %>%
  mutate(country_name_long = ifelse(is.na(country_name_long),"Other",country_name_long)) %>%
  as.vector() %>%
  unlist()

rownames(dt_country) <- rownames(dt_country) %>%
  as.data.frame() %>%
  set_colnames(c("country_code")) %>%
  left_join(aux_country_codes_lenders) %>%
  select(country_name_long) %>%
  mutate(country_name_long = ifelse(is.na(country_name_long),"Other",country_name_long)) %>%
  as.vector() %>%
  unlist()




country_table<-dt_country[rownames(dt_country)%in%names(sort(rowSums(dt_country),decreasing=TRUE))[1:15],
                          colnames(dt_country)%in%names(sort(colSums(dt_country),decreasing=TRUE))[1:20]]

generate_contingency_tables_plots(x = country_table,
                                  file_name = "Country",
                                  lenders_variable = "Country",
                                  loans_variable = "Country")

##on picture ### WIP
dt_picture<-dt_complete %>%
  select(id,starts_with("has_image")) %>%
  left_join(loans[,c('loan_id','pictured_borrowers_num','non_pictured_borrowers_num')],
            by=c("id"="loan_id")) %>%
  mutate(lenders_picture_prop = has_image.yes/(has_image.no+has_image.yes),
         borrowers_picture_prop = pictured_borrowers_num/(pictured_borrowers_num+non_pictured_borrowers_num))
  
prop_pictured_borrowers <- dt_picture$borrowers_picture_prop
prop_pictured_lenders <- dt_picture$lenders_picture_prop
hist_top <- ggplot(dt_gender)+geom_histogram(aes(x = prop_pictured_lenders)) + labs(x ="Proportion of pictured Lenders")
empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())
scatter <- ggplot()+geom_point(aes(x = prop_pictured_lenders, y = prop_pictured_borrowers)) + 
  labs(x ="Proportion of pictured Lenders",
       y = "Proportion of pictured Borrowers")
hist_right <- ggplot()+geom_histogram(aes(x = prop_pictured_borrowers))+ labs(x = "Proportion of pictured Borrowers")+coord_flip() 

p <- grid.arrange(hist_top, 
                  empty, 
                  scatter, 
                  hist_right, 
                  ncol=2, 
                  nrow=2, 
                  widths=c(4, 1), 
                  heights=c(1, 4),
                  top = "Borrower-Lender Relationship: Picture")
ggsave(p,file = "/Users/marcvalenti/TFM/Figures/Contingency/Picture.png")


##on occupation
dt_occupation <- dt_complete %>%
  select(id,
         starts_with("occ_")) %>%
  left_join(loans[,c("loan_id",'sector_name')],by=c("id"="loan_id")) %>%
  select(-id) %>%
  as.data.frame() %>%
  melt(by="sector_name") %>%
  group_by(sector_name,variable) %>%
  summarise(value=sum(value,na.rm=TRUE)) %>%
  filter(value!=0) %>%
  set_colnames(c("borrower","lender","value")) %>%
  dcast(lender~borrower,fun.aggregate=sum)

rownames(dt_occupation) <- unlist(lapply(dt_occupation$lender, function(x) str_replace_all(x,"occ_","")))

dt_occupation$lender<-NULL
sort(rowSums(dt_occupation),decreasing=TRUE)[1:10]
sort(colSums(dt_occupation),decreasing=TRUE)[1:10]

#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/113-ca-correspondence-analysis-in-r-essentials/
occ_table<-t(dt_occupation[rownames(dt_occupation)%in%names(sort(rowSums(dt_occupation),decreasing=TRUE)),
                          colnames(dt_occupation)%in%names(sort(colSums(dt_occupation),decreasing=TRUE))])

generate_contingency_tables_plots(x = occ_table,
                                  file_name = "Occ",
                                  lenders_variable = "Sector",
                                  loans_variable = "Occupation")

# 
# res.ca <- CA(country_table, graph = FALSE)
# print(res.ca)
# fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))
# fviz_ca_biplot(res.ca, repel = TRUE)
# fviz_ca_row(res.ca, repel = TRUE)
# fviz_ca_row(res.ca, col.row = "cos2",
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
#             repel = TRUE)
# library("corrplot")
# corrplot(row$cos2, is.corr=FALSE)
# fviz_ca_row(res.ca, col.row = "contrib",
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
#             repel = TRUE)
# row <- get_ca_row(res.ca)
# fviz_ca_col(res.ca, col.col = "cos2", 
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#             repel = TRUE)
