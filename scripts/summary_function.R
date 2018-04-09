#Calling R libraries

################################
################################
#Workspace 
#####
dir  <- "D:/AICHI13/Huanuco"
trans_dir <- paste0(dir,"/","csv");if(!file.exists(trans_dir)){dir.create(trans_dir)}
plot_dir <- paste0(dir,"/","plot")
################################
#Reading RDS file to be used in clustering

final_table <- readRDS(paste0(trans_dir,"/","cluster_table.RDS"))
clus<-unique(final_table$clust)

tmp<-list()
#i=1
df <- final_table
df2<-df[,2:(ncol(df)-1)]#11

for(i in 1:length(clus)){
  cat("Cluster: ", i,"\n")
  df3<-df2[which(df$clust==clus[[i]]),]
  
  
  tmp[[i]]<-as.data.frame(matrix(nrow=ncol(df3),ncol=7))
  
  colnames(tmp[[i]])<-c("mean","sd","median","min","max","n","cluster")
  row.names(tmp[[i]])<-colnames(df3)
  for(j in 1:(ncol(df3))){
    
    if(is.factor(df3[[j]])) {
      cat("factor: column ",j,"\n")
      
      tmp[[i]][j,1]<-NA
      tmp[[i]][j,2]<-NA
      tmp[[i]][j,3]<-NA
      tmp[[i]][j,4]<-NA
      tmp[[i]][j,5]<-NA
      tmp[[i]][j,6]<-NA
      tmp[[i]][j,7]<-NA
      
      
      
      
    } else {
      cat("numeric: column ",j,"\n")
      tmp[[i]][j,1]<-mean(df3[[j]],na.rm=T)
      tmp[[i]][j,2]<-sd(df3[[j]],na.rm=T)
      tmp[[i]][j,3]<-median(df3[[j]],na.rm=T)
      tmp[[i]][j,4]<-min(df3[[j]],na.rm=T)
      tmp[[i]][j,5]<-max(df3[[j]],na.rm=T)
      tmp[[i]][j,6]<-length(df3[[j]])
      tmp[[i]][j,7]<-as.character(clus[[i]])
      
    }
  };rm(j)
};rm(i)



total<-as.data.frame(matrix(nrow=ncol(df2),ncol=7))

colnames(total)<-c("mean","sd","median","min","max","n","cluster")
row.names(total)<-colnames(df3)


for(i in 1:ncol(df2)){
  
  if(is.factor(df2[[i]])) {
    cat("factor: column ",i,"\n")
    
  } else{
    cat("numeric: column ",i,"\n")
    total[i,1]<-mean(df2[[i]],na.rm=T)
    total[i,2]<-sd(df2[[i]],na.rm=T)
    total[i,3]<-median(df2[[i]],na.rm=T)
    total[i,4]<-min(df2[[i]],na.rm=T)
    total[i,5]<-max(df2[[i]],na.rm=T)
    total[i,6]<-length(df2[[i]])
    total[i,7]<-"Total"
    
    
  }
};rm(i)


final<-do.call("rbind",tmp)
final<-rbind(total,final)
final$item <- row.names(final)
final <- final[,c(8,1:7)]
write.table(final,paste0(trans_dir,"/","SUMMARY_CLUSTER.csv"),quote=F,row.names = F,sep = "|")
