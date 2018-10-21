# library(dplyr)
# library(cluster)
# library(vegan) #dist
# #words needed
source("scripts/libraries_functions.R")
load("/Users/marcvalenti/TFM_data/data_needed.RData")

dt <- dtmr_partner_id %>%
  filter(month<=3) 
y_resp <- dt$partner_id
dt <- dt %>%
  select(-month,-partner_id)

d_euclidean <- dist(dt)
dt_euclidean <- cmdscale(d_euclidean,eig=TRUE,k=600)
# d_manhattan <-vegdist(dt,
#                       method = "manhattan")
# dt_manhattan <- cmdscale(d_manhattan,eig=TRUE,k=600)$x


##how many words do we need?
wf <- dtmr_partner_id %>%
  select(-c(partner_id,month)) %>%
  colSums() %>%
  as.data.frame() %>%
  set_colnames(c("occurences")) 
wf <- wf %>%
  mutate(word = rownames(wf)) %>%
  arrange(-occurences)

threshold_words <- c(2,5,10,50,100,250,500)
is_pca <- c(TRUE,FALSE)
svm <- c("linear","radial")
dt_res <- expand.grid(threshold_words,is_pca,svm)
colnames(dt_res) <- c("n_words_dim","is_pca","svm")
dt_res$time <- NA
dt_res$acc <- NA
conf_matrix_linear <- save_model_linear <- conf_matrix_radial <- save_model_radial <- time_linear <- time_radial <- list()
idx_list_linear <- idx_list_radial <- 1

for (i in 1:nrow(dt_res)){
  #progress(i, progress.bar = TRUE)
  if (dt_res[i,"is_pca"]!=TRUE){
    TrainData <- dtmr_partner_id[,names(dtmr_partner_id) %in% c(wf$word[1:dt_res[i,"n_words_dim"]],"partner_id")]    
    print(paste0("i = ",i,
                 "n_dim = ",dt_res[i,'n_words_dim'],
                 "is_pca = ",dt_res[i,'is_pca']))
  } else {
    TrainData <- dt_euclidean$points[,c(1:dt_res[i,"n_words_dim"])] %>%
      as.data.frame()
    TrainData$partner_id <- y_resp
    print(paste0("i = ",i,
                 "n_dim = ",dt_res[i,'n_words_dim'],
                 "is_pca = ",dt_res[i,'is_pca']))
  }

  TrainData$label <- TrainData$partner_id
  TrainData$partner_id <- NULL
  set.seed(512)
  
  data_part <- createDataPartition(y = TrainData$label, 
                                   p = 0.7, list = F)
  testTrainData <- TrainData[-data_part,] # 30% data goes here
  trainTrainData <- TrainData[data_part,] # 70% here

  X <- trainTrainData[,!names(trainTrainData)%in%c("label")]
  Y <- trainTrainData$label
  Outcome <- as.factor(trainTrainData$label)
  
  TrainCtrl1 <- trainControl(method = "repeatedcv", 
                             number = 10,
                             repeats = 2,
                             verbose = TRUE)
  
  set.seed(512) 
  
  if (dt_res[i,"svm"]=="linear"){
    ct <- Sys.time()
    modelSvmlinear <- train(X,
                            Y, 
                            method="svmLinear", 
                            trControl=TrainCtrl1,
                            # tuneGrid = SVMgrid,
                            preProc = c("scale","YeoJohnson"), 
                            verbose=TRUE)
    dt_res[i,"time"]<- difftime(Sys.time(),ct, units = "secs")
    print(Sys.time() - ct)
    
    save_model_linear[[idx_list_linear]]<-modelSvmlinear
    PredictedTest_linear <- predict(modelSvmlinear,testTrainData[,!names(testTrainData)%in%c("label")])
    conf_matrix_linear[[idx_list_linear]]<- confusionMatrix(testTrainData$label,PredictedTest_linear)
    dt_res[i,"acc"] <- confusionMatrix(testTrainData$label,PredictedTest_linear)["overall"]$overall["Accuracy"]
    idx_list_linear <- idx_list_linear +1
  } else {
    ct <- Sys.time()
    modelSvmRRB <- train(X,
                         Y, 
                         method="svmRadial", 
                         trControl=TrainCtrl1,
                         # tuneGrid = SVMgrid,
                         preProc = c("scale","YeoJohnson"), 
                         verbose=TRUE)
    dt_res[i,"time"]<- difftime(Sys.time(),ct, units = "secs")
    print(Sys.time() - ct)

    save_model_radial[[idx_list_radial]]<-modelSvmRRB
    PredictedTest_radial <- predict(modelSvmRRB,testTrainData[,!names(testTrainData)%in%c("label")])
    conf_matrix_radial[[idx_list_radial]]<- confusionMatrix(testTrainData$label,PredictedTest_radial)
    dt_res[i,"acc"] <- confusionMatrix(testTrainData$label,PredictedTest_radial)["overall"]$overall["Accuracy"]
    idx_list_radial <- idx_list_radial + 1
  }
}

save(save_model_linear,save_model_radial,file = "/Users/marcvalenti/TFM_data/svm_res_2012Q1_10cv.RData")
load("/Users/marcvalenti/TFM_data/svm_res_2012Q1_10cv.RData")

save(dt_res,file = "/Users/marcvalenti/TFM_data/svm_res_2012Q1_10cv_summary.RData")
load("/Users/marcvalenti/TFM_data/svm_res_2012Q1_10cv_summary.RData")

dt_plot <- dt_res %>%
  melt(id.var = c("n_words_dim","is_pca","svm")) %>%
  mutate(label = ifelse(is_pca==TRUE,
                        paste0("PCA - ",svm),
                        paste0("No PCA - ",svm)),
         support_vector_machine_type = svm,
         MDS_reduction = is_pca,
         variable = ifelse(variable=="time","Time","Accuracy"))

plot_metrics <- ggplot(dt_plot,
                       aes(x = n_words_dim,
                           y = value,
                           col = MDS_reduction,
                           group = interaction(MDS_reduction,support_vector_machine_type),
                           linetype = support_vector_machine_type)) + 
  geom_line() + 
  facet_wrap(~variable, scales = "free") + 
  theme_economist_white() + 
  labs(title = "Accuracy and Training Time",
       subtitle = "\t As a function of the number of dimensions, the type of SVM and being reduced using MDS or not \n\t (N=19505), All descriptions for the 2011Q1 in Philippines",
       x = "Number of dimensions used",
       y = "Value")
ggsave(file = "figures/plot_acc_time_svm_descriptions.png",
       plot = plot_metrics)

# modelSvmlinear <- save_model_linear[[11]]
# conf_matrix <- confusionMatrix(testTrainData$label,PredictedTest_linear)
# ConfMat <- as.data.frame.matrix(conf_matrix$table)
# ConfMat_stats <- as.data.frame.matrix(conf_matrix$byClass)
# 
# capture.output(stargazer(ConfMat, colnames = TRUE, rownames = TRUE , title = "Confusion Matrix", summary = FALSE,
#                          out = "/Users/marcvalenti/TFM/Tables/conf_matrix.tex"))
# capture.output(stargazer(ConfMat_stats, out.header = TRUE,colnames = TRUE, title = "Summary Statistics From the Confusion Matrix", summary = FALSE,
#                out = "/Users/marcvalenti/TFM/Tables/conf_matrix_summary.tex"))
