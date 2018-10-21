source("scripts/data_loading/data_loading_loans.R")

dt_philippines<-loans %>%
  mutate(year=as.integer(year(loans$posted_time))) %>%
  filter(country_name=='Philippines')

rm(loans); rm(lenders); rm(loans_lenders)

#obtain and plot the number of loans for partner by year
dt_philippines_partners<- dt_philippines %>%
  group_by(partner_id,year) %>%
  summarise(n_partner=n()) 

#required to obtain the names
# partners_aux<-aggregate(n_partner~partner_id,dt_philippines_partners,FUN=sum) %>%
#   top_n(10,n_partner) %>%
#   mutate(name=NA)
# 
# description<-list()

#does an API request; requiring thus internet access
#alternatively, one can load the "Names_partners.Rda" object
# obtain_description<-function(x){
#   url<-paste0("https://api.kivaws.org/v1/partners/",partners_aux$partner_id[i],".json")
#   url %>% 
#   readLines() %>%
#   fromJSON() %>%
#   return()
# }
# 
# for (i in 1:10){
#   description[i]<-obtain_description(i)
#   partners_aux$name[i]<-description[i][[1]]$name
# }

#save(partners_aux,file="Names_partners.Rda")



dt_philippines_partners<-dt_philippines_partners %>%
  left_join(partners_aux[,c(1,3)],by="partner_id")

ggplot(dt_philippines_partners[!is.na(dt_philippines_partners$name),],aes(x=year,y=n_partner,fill=as.factor(name))) + 
  geom_bar(position='dodge',stat='identity') + 
  theme_economist_white() + 
  scale_color_discrete(name="Partner ID") +  
  #scale_x_continuous(breaks = min(herfindahl_index$year):max(herfindahl_index$year)) + 
  labs(title='Philipinnes: Number of Loans per Partner per Year',
       x='Year',
       y='') + 
  theme(legend.text=element_text(size=8),
        legend.position="right",
        legend.direction="vertical",
        axis.text.x = element_text(size=8)) + 
  guides(fill=guide_legend("Partner Name"))

#dt_philippines <- dt_philippines[dt_philippines$year == 2012,]
sample_size<-nrow(dt_philippines) #works with 100k
idx_sample<-sample(nrow(dt_philippines),sample_size)

docs<-Corpus(VectorSource(dt_philippines$description[idx_sample]))
# remove \n newline, \r carriage return, \t tab character and <br> line break
docs <- tm_map(docs, removeWords, c('n','br','t','r'))
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Text stemming
#docs <- tm_map(docs, stemDocument)
#remove the words that are about the name of the partners
docs <- tm_map(docs, removeWords, c('hspfi','pmpc','gdmpc','nwtf'))
dtmr <- DocumentTermMatrix(docs, 
                          control=list(wordLengths=c(4, 20),
                                       bounds = list(global = c(length(idx_sample)*0.01,
                                                                length(idx_sample)*0.99))))

dtmr_partner_id<-as.data.frame(cbind((dt_philippines$partner_id)[idx_sample],month(dt_philippines$posted_time)[idx_sample],year(dt_philippines$posted_time)[idx_sample],as.matrix(dtmr)))
dtmr_partner_id$partner_id <- as.factor(dtmr_partner_id$V1)
dtmr_partner_id$month <- dtmr_partner_id$V2
dtmr_partner_id$year <- dtmr_partner_id$V3
dtmr_partner_id$V1 <- dtmr_partner_id$V2 <- dtmr_partner_id$V3 <- NULL

kable(as.matrix(dtmr[1:5,1:5]))
#this table shows, for every description, the times of every word appearing
#the dimension is 259776 x 203, being every row every description and every column the count of every word
dim(dtmr)
colSums(as.matrix(dtmr))
save(dtmr_partner_id,file = "/Users/marcvalenti/TFM_data/data_needed_full.RData")
#save(dtmr_partner_id,file = "/Users/marcvalenti/TFM_data/data_needed.RData")
sort(colnames(dtmr_partner_id))
