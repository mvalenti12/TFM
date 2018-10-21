
distr_loans_lenders <- unlist(sapply(loans_lenders[,2],function(x) str_count(x,","))+1) %>%
  as.data.frame()

median <- median(distr_loans_lenders$lender_ids)
p <- distr_loans_lenders[sample(nrow(distr_loans_lenders),100000),] %>%
  as.data.frame() %>%
  set_colnames("lender_ids") %>%
  ggplot() + 
  geom_density(aes(x = lender_ids)) + 
  geom_vline(aes(xintercept = median,
                 col = "red"),
             show.legend = FALSE) + 
  geom_text(aes(x = median+10,
                y = 0.035,
                col = "red",
                label = paste0("Median = ",median)),
            show.legend = FALSE) +
  scale_x_continuous(limits = c(0,100)) + 
  theme_economist_white() +
  labs(title = "Distribution of number of lenders per loan",
       subtitle = paste0("Excluding loans with more than 100 lenders (",round(sum(distr_loans_lenders$lender_ids>100)/nrow(distr_loans_lenders)*100,1),"% of cases)"),
       x = "Number of Lenders")

ggsave("Figures/loans_lenders_distr.png",p)

