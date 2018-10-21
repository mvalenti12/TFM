#how acc changes over time
load("/Users/marcvalenti/TFM_data/data_needed_full.RData")

unique(dtmr_partner_id$month)
unique(dtmr_partner_id$year)


dt <- dtmr_partner_id %>%
  arrange(-year,-month) %>%
  filter(month<=3,
         year == 2011) 
dt$partner_id <- factor(y, levels = sort(as.character(unique(dt$partner_id))))

# Create model with default paramters
control <- trainControl(method="repeatedcv", number=2, repeats=2)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(partner_id~., 
                    data=dt, 
                    method="rf",
                    metric=metric, 
                    tuneGrid=tunegrid, 
                    trControl=control)

p <- varImp(rf_default)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()

which_feat <- varImp(rf_default)$importance %>%
  rownames_to_column() %>%
  arrange(-Overall)
