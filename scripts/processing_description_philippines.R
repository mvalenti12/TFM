
source("scripts/dtmr_generation.R")
# cor(as.vector(dtmr[1:2,]),as.vector(dtmr[30:31,]))
# 
# dtmr_small <-DocumentTermMatrix(docs, 
#                                 control=list(wordLengths=c(4, 20),
#                                              bounds = list(global = c(nrow(dt_philippines)*0.20,
#                                                                       nrow(dt_philippines)*0.5))))
# 
# 
# dtmr_small$dimnames
load("/Users/marcvalenti/TFM_data/data_needed.RData")
dtmr <- dtmr_partner_id %>%
  select(-partner_id)
colnames(dtmr_partner_id)
freqr <- apply(dtmr,2,sum)
# 
# findFreqTerms(dtmr,lowfreq=80)
# 
# findAssocs(dtmr,"business",0.6)

wf=data.frame(term=colnames(dtmr),occurrences=freqr)
wf$term<-as.character(wf$term)

ggplot(subset(wf, freqr>mean(freqr)), aes(term, occurrences)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  theme_economist() + 
  labs(title='Frequency of words',
       ylab="")

wordcloud(names(freqr),freqr, min.freq=0)

#https://www.tidytextmining.com/tfidf.html
# partner_id_sum<-aggregate(.~ V1, data = dtmr_partner_id, sum)
# partner_id_sum_long<-melt(partner_id_sum,id="V1")
# colnames(partner_id_sum_long)<-c("partner_id","word","n")
# 
# partner_id_sum_long<-partner_id_sum_long %>%
#   group_by(partner_id) %>%
#   mutate(total=sum(n)) %>%
#   filter(!partner_id%in%c(351,389,409,508)) %>%
#   left_join(partners_aux[,c(1,3)],by="partner_id")
# 
# ggplot(partner_id_sum_long, aes(n/total, fill = word)) +
#   geom_histogram(show.legend = FALSE) +
#   #xlim(NA, 0.0009) +
#   facet_wrap(~name, scales = "free_y") + 
#   #theme(strip.text = element_text(size=5)) + 
#   labs(title="Frequency of frequencies for every partner",
#        xlab="Frequency",
#        ylab="Count") + 
#   theme_economist()
# 
# 
# 
# 
# partner_id_tf_idf <- partner_id_sum_long %>%
#   left_join(wf,by=c('word'='term')) %>%
#   mutate( tf=n/total,
#           idf=log(dim(dtmr)[1]/occurrences),
#           tf_idf=tf*idf) %>%
#   arrange(-tf_idf)
# 
# 
# partner_id_tf_idf %>%
#   arrange(desc(tf_idf)) %>%
#   mutate(word = factor(word, levels = rev(unique(word)))) %>% 
#   group_by(partner_id) %>% 
#   top_n(10,tf_idf) %>% 
#   ungroup %>%
#   arrange(tf_idf) %>%   
#   left_join(partners_aux[,c(1,3)],by="partner_id") %>% 
#   ggplot(aes(word, tf_idf, fill = name.x)) +
#   labs(title='Words of Every Partner by IDF',
#        y='Term Frequency - Inverse Document Frequency') + 
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~name.x, ncol = 2, scales = "free") +
#   coord_flip() +
#   theme_economist_white()
# 
# sentiment<-partner_id_sum_long %>%
#   inner_join(get_sentiments("bing"),by="word")
# 
# sentiment %>%
#   filter(n >= 200) %>%
#   mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n, fill = sentiment)) +
#   geom_bar(stat = "identity") +
#   coord_flip() + 
#   theme_economist_white() + 
#   labs(title='Contribution of the individual words to the sentiment library',
#        ylab='Contribution to sentiment')
# 
# 
# sentiment_nrc<-partner_id_sum_long %>%
#   inner_join(get_sentiments("nrc"),by="word") %>%
#   group_by(partner_id,sentiment) %>%
#   summarise(value=sum(n))
# 
# sentiment_individual_nrc<-(as.matrix(dtmr)) %>%
#   melt(id.var=c("Docs","partner_id")) %>%
#   inner_join(get_sentiments("nrc"),by=c("Terms"="word")) %>%
#   group_by(Docs,sentiment) %>%
#   summarise(value=sum(value)) %>%
#   dcast(Docs~sentiment) %>%
#   cbind(dt_philippines$partner_id) %>%
#   set_colnames(c("partner_id","variable","value")) %>%
#   filter(variable!='Docs',
#          !dt_philippines$partner_id%in%c(351,389,409,508)) 
# 
# 
# 
# 
# sentiment_individual_nrc<-(as.matrix(dtmr)) %>%
#   melt(id.var=c("Docs","partner_id")) %>%
#   inner_join(get_sentiments("nrc"),by=c("Terms"="word")) %>%
#   group_by(Docs,sentiment) %>%
#   summarise(value=sum(value)) %>%
#   dcast(Docs~sentiment) %>%
#   cbind(dt_philippines$partner_id) %>%
#   melt(id.var="dt_philippines$partner_id") %>%
#   set_colnames(c("partner_id","variable","value")) %>%
#   filter(variable!='Docs',
#          !dt_philippines$partner_id%in%c(351,389,409,508))
# 
# 
# 
# ggplot(sentiment_individual_nrc,
#        aes(value,colour=as.factor(partner_id))) +
#   geom_density(alpha = 0.1) + 
#   facet_wrap(~variable,scales="free") + 
#   theme_economist() + 
#   labs(title='Differences in marginal distributions by partner id')
# 
# 
# 
# dt_philippines_idx<-dt_philippines %>%
#   mutate(idx=rownames(dt_philippines)) %>%
#   select(partner_id,idx,funded_time) %>%
#   filter(!partner_id%in%c(351,389,409,508)) %>%
#   group_by(partner_id) %>%
#   arrange(partner_id,desc(funded_time)) %>%
#   top_n(n=200,
#         wt=desc(funded_time)) %>%
#   select(-funded_time)


