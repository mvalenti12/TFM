source("/Users/marcvalenti/TFM/scripts/libraries_functions.R")
load("/Users/marcvalenti/TFM_data/dt_loans_reduced_philippines.RData")
load("/Users/marcvalenti/TFM_data/image_recognition.RData")
res_google <- as.data.frame(res_google)
res_azure <- as.data.frame(res_azure)
dt$time_to_fund <- as.numeric(as.POSIXct(dt$raised_time)-as.POSIXct(dt$posted_time))/60
dt$day_week <- strftime(as.POSIXct(dt$posted_time, tz = "America/Los_Angeles"),'%A',tz = "America/Los_Angeles")

colnames(res_azure) <-  c("A_anger",
                          "A_contempt",
                          "A_disgust",
                          "A_fear",
                          "A_happiness",
                          "A_neutral",
                          "A_sadness",
                          "A_surprise")

colnames(res_google) <- c("detectionConfidence",
                          "landmarkingConfidence",
                          "G_joy",
                          "G_sorrow",
                          "G_anger",
                          "G_surprise",
                          "G_underExposed",
                          "G_blurred",
                          "G_headwear")
res_azure %>%
  melt() %>%
  ggplot(aes(x = variable,
             y = value,
             group = variable)) + 
  geom_boxplot() + 
  theme_economist_white() + 
  labs(title = "Microsoft's Azure Output (Boxplot)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(text = element_text(size = 20)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 20)) +
  ggsave("/Users/marcvalenti/TFM/Figures/C5_images/plot_azure_out.png",
         width = 12,
         height = 9)


res_azure_table <- summary(res_azure) %>%
  as.data.frame() %>%
  mutate(category = as.character(Var2),
         variable = as.character(Freq)) %>%
  select(-Var1,-Var2,-Freq) %>%
  mutate(label = str_split_fixed(variable,
                fixed(":"),2)[,1],
         value = as.numeric(str_split_fixed(variable,
                                         fixed(":"),2)[,2])) %>%
  select(-variable) %>% 
  dcast(category ~ label) %>%
  mutate(higher_001 = colSums(res_azure>0.01,na.rm = TRUE),
         higher_01 = colSums(res_azure>0.1,na.rm = TRUE)) %>%
  column_to_rownames(var = "category") 

res_azure_table <- res_azure_table[,c(6,1,5,4,2,3,9,8,7)]
# Commenting because there is a manual edit
# res_azure_table %>%
#   as.data.frame() %>%
#   xtable(caption = "Summary of Azure Output",
#          label = "Tab:Img_Azure") %>%
#   print(file = "/Users/marcvalenti/TFM/Tables/table_img_out_azure.tex")

res_google_table <- matrix(NA,
                           ncol = 5,
                           nrow = length(names(res_google)[startsWith(names(res_google),"G_")])) %>%
  as.data.frame() %>%
  set_rownames(names(res_google)[startsWith(names(res_google),"G_")]) %>%
  set_colnames(c("VERY_UNLIKELY","UNLIKELY","POSSIBLE","LIKELY","VERY_LIKELY"))

for (k in 1:nrow(res_google_table)){
  for (j in 1:ncol(res_google_table)){
    res_google_table[k,j] <- sum(res_google[,which(names(res_google)%in%rownames(res_google_table)[k])]==colnames(res_google_table)[j],na.rm=TRUE)
  }
}

res_google_table %>%
  rownames_to_column(var="variable_emotion") %>%
  melt(id.var = "variable_emotion") %>%
  ggplot(aes(x = variable,
             y = variable_emotion,
             label = value,
             fill = value)) + 
  geom_tile(show.legend = FALSE) + 
  geom_text(col = "white",
            size = 10) + 
  theme_economist_white() + 
  labs(title = "Google's Vision Output (Table)",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(text = element_text(size = 20)) + 
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 20)) +
  ggsave("/Users/marcvalenti/TFM/Figures/C5_images/plot_google_out.png",
         width = 12,
         height = 9)
  
rm(j);rm(k)
res_google_table$is_NA <- as.numeric(colSums(is.na(res_google[startsWith(names(res_google),"G_")])))

# res_google_table %>%
#   as.data.frame() %>%
#   xtable(caption = "Summary of Google Output",
#          label = "Tab:Img_Google",
#          digits = 0) %>%
#   print(file = "/Users/marcvalenti/TFM/Tables/table_img_out_Google.tex")
                
res <- cbind(res_azure[1:i,],res_google[1:i,]) %>%
  as.data.frame() 

google_factor_to_numeric <- function(x){
  if (is.na(x)){
    return(NA)
  } else if (x=="VERY_UNLIKELY") {
    return(0)
  } else if (x=="UNLIKELY") {
    return(0.25)
  } else if (x=="POSSIBLE") {
    return(0.5)
  } else if (x=="LIKELY") {
    return(0.75)
  } else if (x=="VERY_LIKELY") {
    return(1)
  } else {
    return(NA)
  }
}
vbles_to_transform <- which(startsWith(names(res),"G_"))

res[,startsWith(names(res),"A_")] <- lapply(res[,startsWith(names(res),"A_")], function(x) as.numeric(as.character(x)))
res[,endsWith(names(res),"Confidence")] <- lapply(res[,endsWith(names(res),"Confidence")], function(x) as.numeric(as.character(x)))
res[,startsWith(names(res),"G_")] <- lapply(res[,startsWith(names(res),"G_")], function(x) as.character(x))
for (i in 1:nrow(res)){
  for (j in 1:length(vbles_to_transform)){
    idx_vble <-vbles_to_transform[j] 
    res[i,idx_vble] <- google_factor_to_numeric(as.character(res[i,idx_vble]))
  }
}
res[,startsWith(names(res),"G_")] <- lapply(res[,startsWith(names(res),"G_")], function(x) as.numeric(x))

# saves table with all the output
# since some modifications are done, the table in Tables/table_img_retrieving.tex is edited manually
# but based on some code generated here
cbind(ifelse(is.na(res$A_disgust),"no","yes"),ifelse(is.na(res$detectionConfidence),"no","yes")) %>%
  as.data.frame() %>%
  set_colnames(c("Azure","Google")) %>%
  table() %>%
  xtable()
data.frame(res$detectionConfidence) %>%
  set_colnames(c("detectionConfidence")) %>%
  summary() %>%
  as.data.frame() %>%
  mutate(category = as.character(Var2),
       variable = as.character(Freq)) %>%
  select(-Var1,-Var2,-Freq) %>%
  mutate(label = str_split_fixed(variable,
                                 fixed(":"),2)[,1],
         value = as.numeric(str_split_fixed(variable,
                                            fixed(":"),2)[,2])) %>%
  select(-variable,-category) %>% 
  xtable(include.rownames=FALSE)

dt_final <- res %>%
  rownames_to_column(var = "loan_id") %>%
  mutate(loan_id = as.integer(loan_id)) %>%
  inner_join(dt[,c("loan_id","time_to_fund","loan_amount","day_week","sector_name")], by = "loan_id") %>%
  # filter(detectionConfidence>0.9) %>%
  select(-detectionConfidence,
         -landmarkingConfidence,
         -G_anger,
         -G_underExposed,
         -G_blurred,
         -G_surprise,
         -G_headwear,
         -loan_id)

dt_final <- dt_final[rowSums(is.na(dt_final))==0,]
dt_std <- scale(dt_final[startsWith(names(dt_final),"G_")|startsWith(names(dt_final),"A_")],
                center = TRUE,
                scale = TRUE)
# prcomp_res <- prcomp(dt_final[startsWith(names(dt_final),"G_")|startsWith(names(dt_final),"A_")],
#                      scale = TRUE)
# biplot(prcomp_res)
# cor(dt_final$G_joy,
#     dt_final$A_happiness)
# plot(prcomp_res)

#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

dt_final$is_monday <- as.numeric(dt_final$day_week=="Monday")
dt_final$is_retail <- as.numeric(dt_final$sector_name=="Retail")

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/
# res.MFA <- MFA (base = dt_std, 
#                  group = c(8,2),
#                  type = rep("s",length(c(8,2))), 
#                  ind.sup = NULL, 
#                  name.group = c("Azure","Google"),
#                  num.group.sup = NULL,
#                 graph = FALSE)

res.MFA <- stats::factanal(x = dt_std,
         factors = 4,
         rotation = "varimax")
loadings <- res.MFA$loadings %>%
  unclass() 

res.MFA$loadings

loadings_dt <- loadings %>%
  as.data.frame() %>%
  rownames_to_column(var = "factor") 

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

dt_circle <- circleFun(diameter = 2,npoints = 100)

p <- ggplot() + 
  geom_text(data = loadings_dt,
             aes (x = Factor1,
                  y = Factor2,
                  label = factor),
            nudge_y = 0.1) +
  geom_segment(data = loadings_dt,
            aes (x = 0,
                 xend = Factor1,
                 y = 0,
                 yend = Factor2),
            size = 0.5,
            arrow = arrow()) +
  scale_x_continuous(limits = c(-1.1,1.1)) + 
  scale_y_continuous(limits = c(-1.1,1.1)) +
  labs(title = "Exploratory Factor Analysis: Loadings",
       x = "First Factor (26.4%)",
       y = "Second Factor (13.4%)") + 
  geom_path(data = dt_circle,
            aes(x = x,
                y = y))
ggsave(p,
       filename = "/Users/marcvalenti/TFM/Figures/C5_Images/FA_loadings.png")
# loadings %>%
#   xtable(caption = "Loadings of the Exploratory Factor Analysis",
#          label = "Tab:Load") %>%
#   print(file = "/Users/marcvalenti/TFM/Tables/table_img_loadings.tex")
# 
# png(filename="/Users/marcvalenti/TFM/Figures/C5_Images/MFA_Var.png")
# parallel <- psych::fa.parallel(dt_std, fm = 'minres', fa = 'fa')
# dev.off()

# fviz_mfa_var(res.MFA) +
#   theme_economist_white() + 
#   ggsave(filename = "/Users/marcvalenti/TFM/Figures/C5_Images/MFA_Var.png")
# 
# 
# fviz_eig(res.MFA) + 
#   theme_economist_white() + 
#   ggsave(filename = "/Users/marcvalenti/TFM/Figures/C5_Images/MFA_Eig.png")

corrplot(cor(dt_std),
         method = "number",
         type = "upper") 
# get_mfa_var(res.MFA)$coord %>%
#   as.data.frame() %>%
#   xtable(caption = "Loadings of the Exploratory Factor Analysis",
#          label = "Tab:Load") %>%
#   print(file = "/Users/marcvalenti/TFM/Tables/table_img_loadings.tex")

dt_ind_coord <- get_mfa_ind(res.MFA)$coord %>%
  as.data.frame() %>%
  set_colnames(c("FA_D1","FA_D2","FA_D3","FA_D4","FA_D5"))

dt_sum <- dt_final %>%
  filter(time_to_fund>0,
         loan_amount<600) %>%
  mutate(happiness = G_joy+A_happiness-A_neutral,
         negative_others = A_anger + A_disgust + G_sorrow + A_sadness + A_contempt + A_fear) %>%
  select(time_to_fund,
         loan_amount,
         is_monday,
         is_retail,
         happiness,
         negative_others)

ggplot(dt_final,
         aes(x = time_to_fund)) + 
  geom_density()
ggplot(dt_sum,
       aes(x = time_to_fund,
           y = loan_amount)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  scale_x_log10() + 
  scale_y_log10()


res_lm <- lm((time_to_fund) ~ loan_amount^2 + is_monday + is_retail + happiness + negative_others,
             data = dt_sum)
res_lm$aic <- AIC(res_lm)
res_lm$bic <- BIC(res_lm)
summary(res_lm)
plot(res_lm)

res_log_lm <- lm(log(time_to_fund) ~ loan_amount + is_monday + is_retail + happiness + negative_others,
             data = dt_sum)
res_log_lm$aic <- AIC(res_log_lm)
res_log_lm$bic <- BIC(res_log_lm)
summary(res_log_lm)
plot(res_log_lm)

res_glm <- glm((time_to_fund) ~ .,
                  data = dt_sum)
res_glm$aic <- AIC(res_glm)
res_glm$bic <- BIC(res_glm)
summary(res_glm)
plot(res_glm)


dt_sum_fa <- cbind(dt_final,dt_ind_coord) %>%
  filter(time_to_fund>0,
         loan_amount<600) %>%
  select(time_to_fund,
         loan_amount,
         is_monday,
         is_retail,
         FA_D1,
         FA_D2,
         FA_D3)

res_lm_fa <- lm((time_to_fund) ~ .,
             data = dt_sum_fa)
res_lm_fa$aic <- AIC(res_lm_fa)
res_lm_fa$bic <- BIC(res_lm_fa)
summary(res_lm_fa)
plot(res_lm)

res_log_lm_fa <- lm(log(time_to_fund) ~ .,
                data = dt_sum_fa)
res_log_lm_fa$aic <- AIC(res_log_lm_fa)
res_log_lm_fa$bic <- BIC(res_log_lm_fa)
summary(res_log_lm_fa)
plot(res_log_lm_fa)

res_glm_fa <- glm((time_to_fund) ~ .,
                data = dt_sum_fa)
res_glm_fa$aic <- AIC(res_glm_fa)
res_glm_fa$bic <- BIC(res_glm_fa)
summary(res_glm_fa)
plot(res_glm_fa)



png(filename="/Users/marcvalenti/TFM/Figures/C5_images/img_validation_log_lm.png")
par(mfrow=c(2,2))
plot(res_log_lm_fa)
dev.off()


plot(dt_sum_fa$time_to_fund,dt_sum_fa$loan_amount)


# 
# writeLines(capture.output(stargazer(res_lm,res_log_lm,res_glm,res_lm_fa,res_log_lm_fa,res_glm_fa,
#                                     add.lines = list(c("AIC",
#                                                        round(AIC(res_lm),1),
#                                                        round(AIC(res_log_lm),1),
#                                                        round(AIC(res_glm),1),
#                                                        round(AIC(res_lm_fa),1),
#                                                        round(AIC(res_log_lm_fa),1),
#                                                        round(AIC(res_glm_fa),1)),
#                                                      c("BIC",
#                                                        round(BIC(res_lm),1),
#                                                        round(BIC(res_log_lm),1),
#                                                        round(BIC(res_glm),1),
#                                                        round(BIC(res_lm_fa),1),
#                                                        round(BIC(res_log_lm_fa),1),
#                                                        round(BIC(res_glm_fa),1))))),
#            "/Users/marcvalenti/TFM/Tables/lm_img.tex")
# 



shapiro.test(as.vector(res_log_lm$residuals))
# From the output, the p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.
# but we have a lower p-value lol

# \resizebox{\textwidth}{!}{\begin{tabular}{@{\extracolsep{5pt}}lcccc}
# \end{tabular}}
mod <- gvlma::gvlma(res_log_lm)
summary(mod)
mod
plot(mod)
