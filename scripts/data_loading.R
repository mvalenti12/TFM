source("scripts/libraries_functions.R")

source("scripts/data_loading/data_loading_loans.R")
source("scripts/data_loading/data_loading_lenders.R")
source("scripts/data_loading/data_loading_loans_lenders.R")

aux_country_codes <- loans %>%
  filter(!is.na(country_code)) %>%
  distinct(country_code,country_name) 

#This metadata from the countries is just to group the country into regions or continents level.
# countries_metadata<-read.csv("countries_metadata.csv",sep=";")
# countries_metadata$Country<-as.character(countries_metadata$Country)

##needed later aswell
# world.map <- readOGR(dsn="data", layer="TM_WORLD_BORDERS_SIMPL-0.3")
# world.ggmap <- fortify(world.map, region = "NAME")
#save(countries_metadata,world.ggmap,partners_aux,file="Auxiliary_files.Rdata")

load("/Users/marcvalenti/TFM_data/Auxiliary_files.Rdata")






