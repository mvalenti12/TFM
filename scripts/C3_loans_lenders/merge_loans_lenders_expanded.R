# this two scripts generate the .RData files to run this script.
# source("scripts/expanding_info_loans_lenders/info_loans.R")
# source("scripts/expanding_info_loans_lenders/info_lenders.R")

load("/Users/marcvalenti/TFM_data/loans_info_expanded.RData")
load("/Users/marcvalenti/TFM_data/lenders_info_expanded_hot.RData")
source("scripts/data_loading/data_loading_loans_lenders.R")



obtain_lenders_data<-function(lenders_vector){
  aux_lenders<-(unlist(str_split(gsub(' ','',lenders_vector),','))) %>%
    as.data.table() %>%
    set_colnames(c("id"))
  aux_lenders[lenders_info_expanded_hot,on="id",nomatch=0] %>%
    select(-id) %>%
    colSums(na.rm=TRUE) %>%
    as.vector() %>%
    return()
}

# Use the detectCores() function to detect the number of CPU cores on the current host.
no_cores <- detectCores()

# Creates a set of copies of R running in parallel and communicating over sockets.
clust <- makeCluster(no_cores) 

# clusterExport assigns the values on the master R process of the variables named in varlist to variables of the same names in the global environment (aka ‘workspace’) of each node. The environment on the master from which variables are exported defaults to the global environment.
# we export all the nedded variables
clusterExport(clust, "obtain_lenders_data")
clusterExport(clust, "%>%")
clusterExport(clust, "str_split")
clusterExport(clust, "as.data.table")
clusterExport(clust, "set_colnames")
clusterExport(clust, "lenders_info_expanded_hot")
clusterExport(clust, "select")

t <- Sys.time()
# parSapply is a parallell version of Sapply.
dt_complete <- t(parSapply(clust,
                           loans_lenders$lenders[200001:250001], 
                           obtain_lenders_data,
                           USE.NAMES = FALSE))
Sys.time() - t

stopCluster(clust)

colnames(dt_complete)<-colnames(lenders_info_expanded_hot)[-1]
rownames(dt_complete)<-loans_lenders$loan_id[200001:250001]
dt_complete200_250 <- dt_complete
# save(dt_complete200_250,file = "/Users/marcvalenti/TFM_data/loans_by_lenders200001_250001.RData")
# # 
# # save(dt_complete_200000,file = "/Users/marcvalenti/TFM_data/loans_by_lenders200000.RData")
# # 
# # 
# load("/Users/marcvalenti/TFM_data/loans_by_lenders200000.RData")
# dt_complete_250000 <- rbind(dt_complete_200000,
#                             dt_complete200_250)
# save(dt_complete_250000,file = "/Users/marcvalenti/TFM_data/loans_by_lenders250000.RData")
