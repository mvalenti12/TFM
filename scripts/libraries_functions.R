#required libraries

#rendering doc
require(knitr)
require(markdown)
library(tinytex)
require(xtable)

#data importing
library(jsonlite) 
library(readr) 
library(data.table)
#data manipulation
library(dplyr) 
library(stringr)
library(tibble) #rownames_to_column
library(lubridate)
library(stringr)
library(reshape)
library(reshape2)
library(magrittr)
library(multiplex) #writting .dat files

library(svMisc) #progressbar
#data visualization
library(ggplot2)
library(ggthemes)
library(scales)
library(gridExtra)
library(grid)
library(ggpubr)
library(rworldmap)
library(gpclib)
library(rgeos)
library(maptools)
library(gg3D) #devtools::install_github("AckerDWM/gg3D")
library(knitr)
library(rgdal)
library(ca)
library(psych)
library(corrplot)
#web scrapping
library(rvest)
library(tidyverse)
library(httr)

library(RoogleVision)

#text mining
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

library(vegan) #dist
#predicting
library(naivebayes)
library(e1071)
library(caret)
library(mlbench)

#parallel computing
library(parallel)   

#correspondence analysis
library("FactoMineR")
library("factoextra")
library("gplots")

#text mining
library(tidytext)
library(text2vec)

options(scipen = 999)

#latex publishing
library(stargazer)

# setwd("~/Desktop/kiva_ds_json")
# load("Auxiliary_files.Rdata")

to_date <- function(x){
  return(as.Date(as.POSIXct(x,origin='1970-01-01')))
}

write_to_txt <- function(x , file_fun){
  if (!file.exists(file_fun)){
    file.create(file_fun)
  }
  cat(paste(deparse(substitute(x)), " = ",x), file = file_fun, sep = "\n",append = TRUE)
  file.show(file_fun)
}

print_latex_table <- function(x,
                              table_header,
                              table_title,
                              table_reference,
                              n_digits = 2,
                              number,
                              align_fun = "lllll"
){
  latex_table <- table(x, exclude = NULL) %>%
    as.data.frame() %>%
    arrange(-Freq) %>%
    mutate(relative_freq = round(Freq / sum (Freq, na.rm = FALSE)*100,n_digits)) %>%
    filter(!is.na(x)) %>%
    mutate(relative_freq_perc = paste0(relative_freq,"%")) %>%
    mutate(cumulative_freq = paste0(round(cumsum(as.numeric(relative_freq)),n_digits),"%")) %>%
    select(-relative_freq) %>%
    head(number) %>%
    set_colnames(c(table_header,"Absolute Value","Frequency (%)", "Cumulative Frequency"))
  options(xtable.table.placement="!htb")
  print(xtable(latex_table, 
               type = "latex", 
               align = align_fun,
               caption = table_title, 
               label = paste0("tab:",table_reference)), 
        file = paste0("/Users/marcvalenti/TFM/Tables/table_",
                      str_replace_all(table_reference," ","_"),
                      ".tex"),
        include.rownames=FALSE)
  print(latex_table)
}



print_latex_contingency_table <- function(x,
                                          table_title,
                                          table_reference,
                                          n_digits = 2,
                                          number
){
  latex_table <- x 
  latex_table <- rbind(latex_table,
                       as.vector(apply(x, 2, sum)))
  rownames(latex_table)[nrow(latex_table)] <- "TOTAL"
  options(xtable.table.placement="!htb")
  print(xtable(latex_table, 
               type = "latex", 
               caption = table_title, 
               label = paste0("tab:",table_reference)), 
        file = paste0("/Users/marcvalenti/TFM/Tables/Contingency_Tables/table_contingency_",
                      str_replace_all(table_reference," ","_"),
                      ".tex"),
        include.rownames=FALSE)
  print(latex_table)
}
