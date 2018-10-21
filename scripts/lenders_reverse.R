log_con <- file("log.txt",open="a")
#començant pel final:

obtain_lenders <- function(dt,dt_counter,x){
  aux_lenders<-(unlist(str_split(gsub(' ','',(dt[x,'lenders'])),','))) %>%
    as.data.table() %>%
    set_colnames(c("id"))
  return(aux_lenders[dt_counter,on="id",nomatch=0])
}

final_lenders_original <- lenders %>%
  arrange(member_since) %>%
  select(id,loan_count) %>%
  filter(loan_count>0,
         !is.na(loan_count)) %>%
  as.data.table(key="id")

final_loans_lenders <- loans_lenders %>%
  inner_join(loans[,c('loan_id','raised_time','funded')],
             by=c("loan_id"="loan_id")) %>%
  arrange(desc(raised_time)) %>%
  as.data.table(key="loan_id")

final_res <- matrix(0,ncol=14,nrow=nrow(final_loans_lenders))

for (i in 1:nrow(final_loans_lenders)){
  aux_lenders<-obtain_lenders(final_loans_lenders,final_lenders_original,i)
  final_res[i,]<-as.vector(table(cut(aux_lenders$loan_count,breaks=c(0,1,2,3,4,5,6,7,8,9,10,15,20,50,99999))))
  final_lenders_original[aux_lenders,on="id",loan_count:=loan_count-1] #afegeix 1 al counter
  
  if(i%%100==0){
    final_lenders_original<-final_lenders_original[loan_count>0,]
    cat(paste0("progress: ",i,"(",round(i/nrow(final_loans_lenders)*100,2),"%)"), file = log_con, sep="\n")
    cat(paste0("length lenders dataset:",nrow(final_lenders_original)),file = log_con, sep="\n")
    cat((Sys.time()-t),file=log_con,sep="\n")
    write.csv2(cbind(final_loans_lenders$loan_id[1:i],final_res[1:i,]),file="results.final.csv",row.names = FALSE)
  }
  t<-Sys.time()
}
