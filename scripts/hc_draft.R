#how many do we include and how does it affects the results
#case for only 100 words
n_partners <- length(levels(dt$partner_id))

res_hc <- dt %>%
  select(-partner_id) %>%
  dist() %>%
  hclust()

res_pam <- dt %>%
  select(-partner_id) %>%
  dist() %>%
  pam(n_partners)

res_kmeans <- dt %>%
  select(-partner_id) %>%
  dist() %>%
  kmeans(centers = n_partners)

plot(res_pam)


table(dt$partner_id,cutree(res_hc,k = n_partners))
table(dt$partner_id,res_pam$clustering)
res <- table(dt$partner_id,res_kmeans$cluster)

dim(res)

max_partner <- apply(res,MARGIN=1,max) %>%
  as.data.frame() %>%
  set_colnames(c("partner_predictions")) 
max_partner <- max_partner %>%
  mutate(partner_id = rownames(max_partner)) %>%
  arrange(-partner_predictions)



silhouette(hclustering)

groups <- cutree(hclustering, k = length(unique(dtmr_partner_id$partner_id)))
table(dt$partner_id,groups)

dt_philippines$original_description[4]
sort(colSums(dtmr_partner_id[,!names(dtmr_partner_id)%in%c("partner_id")]))