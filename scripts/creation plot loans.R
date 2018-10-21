#creation world maps
source("/Users/marcvalenti/TFM/scripts/data_loading/data_loading_loans.R")
load("/Users/marcvalenti/TFM_data/Auxiliary_files.Rdata")

#get the number of loans per country
dt_country_level<-loans %>%
  group_by(country_name) %>%
  summarise(funding_rate=sum(funded)/n(),
            loans=n()/1000) 

#required to do the world map
world_countries<-unique(world.ggmap$id)
world_countries_not_in<-world_countries[!world_countries%in%dt_country_level$country_name] %>%
  as.data.frame() %>% 
  cbind(rep(NA,length(world_countries[!world_countries%in%dt_country_level$country_name])),rep(NA,length(world_countries[!world_countries%in%dt_country_level$country_name]))) %>%
  set_colnames(c("country_name","funding_rate","loans"))

dt_country_level<-rbind(dt_country_level,world_countries_not_in)


# plot_funding_rate_country<-ggplot(dt_country_level ,aes(map_id=country_name,fill=(funding_rate))) +
#   labs(title='Kiva Funding Rate per Country') + 
#   geom_map(map =world.ggmap) +
#   expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
#   theme_economist() + 
#   scale_fill_gradient(low = "red", high = "green",limits=c(0,1), labels = percent, name='Funding Rate') + 
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.y= element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position="bottom",
#         legend.key.width = unit(3,'line'))
# 
# plot_funding_rate_country

plot_loans_country<-ggplot(dt_country_level ,aes(map_id=country_name,fill=(loans))) +
  labs(title='Loans received by Country') + 
  geom_map(map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  theme_economist() + 
  scale_fill_gradient(low = "lightblue", high = "darkblue",name='Loans (thousands)') + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y= element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        #plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="bottom",
        legend.key.width = unit(3,'line'))

class(plot_loans_country)

ggsave("Figures/plot_loans_country.png",plot_loans_country)

rm(lplot_loans_country,dt_country_level,world_countries,world_countries_not_in)

###Lenders


#get the number of lenders per country
dt_country_level<-lenders %>%
  group_by(country_code) %>%
  summarise(lenders=n()) %>%
  inner_join(aux_country_codes,
             by = "country_code")

#required to do the world map
world_countries<-unique(world.ggmap$id)
world_countries_not_in<-world_countries[!world_countries%in%dt_country_level$country_code] %>%
  as.data.frame() %>% 
  cbind(rep(NA,length(world_countries[!world_countries%in%dt_country_level$country_name])),rep(NA,length(world_countries[!world_countries%in%dt_country_level$country_name]))) %>%
  set_colnames(c("country_name","funding_rate","loans"))

dt_country_level<-rbind(dt_country_level,world_countries_not_in)

