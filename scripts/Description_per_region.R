#Calling R libraries
require(FactoMineR);require(factoextra);require(cluster);require(NbClust);library(dendextend)

################################
################################
#Workspace 
#####
dir  <- "E:/Huanuco"
trans_dir <- paste0(dir,"/","csv");if(!file.exists(trans_dir)){dir.create(trans_dir)}
trans_dir <- paste0(dir,"/","csv");if(!file.exists(trans_dir)){dir.create(trans_dir)}
trans_process_dir <- paste0(trans_dir,"/","processing");if(!file.exists(trans_process_dir)){dir.create(trans_process_dir)}

trans_results_dir <- paste0(trans_dir,"/","results");if(!file.exists(trans_results_dir)){dir.create(trans_results_dir)}

plot_dir <- paste0(dir,"/","plot");if(!file.exists(plot_dir)){dir.create(plot_dir)}
plot_cluster_dir <- paste0(plot_dir,"/","cluster");if(!file.exists(plot_cluster_dir)){dir.create(plot_cluster_dir)}
plot_full_dir <- paste0(plot_dir,"/","full");if(!file.exists(plot_full_dir)){dir.create(plot_full_dir)}
plot_reg_dir <- paste0(plot_dir,"/","region");if(!file.exists(plot_reg_dir)){dir.create(plot_reg_dir)}
plot_descrip_dir <- paste0(plot_dir,"/","description");if(!file.exists(plot_descrip_dir)){dir.create(plot_descrip_dir)}
#################################
#Reading RDS file to be used in clustering
#http://factominer.free.fr/course/doc/MFA_course_slides.pdf
tab3 <- readRDS(paste0(trans_results_dir,"/","firstStep.RDS"))
final_table <- readRDS(paste0(trans_results_dir,"/","cluster_table.RDS"))


regions <- unique(final_table$region)


#################################
#Ommiting variables with NAs
#c("alt_mean","wealthindex","inc","per_sale")
################################

tab3_alt <- tab3[,-c(2,19,21,22,25)]
for(i in 1:ncol(tab3_alt)){
  cat("Column: ",i," | ",length(tab3_alt[i][is.na(tab3_alt[,i]),]),"\n")
}
tab3_alt <- tab3_alt[complete.cases(tab3_alt),]

tab3_alt2 <-tab3_alt

################################
tab3_alt2 <- tab3_alt2[,c(2:8,1,9:27)]
################################
#Fisher exact test and graphic
################################
tab3_alt2_copy <-tab3_alt2
################################
#Calculating correlations for numerical variables

for(i in 1:length(regions)){
region <- regions[[i]]
tab3_alt2 <-tab3_alt2_copy

tab3_alt2 <- tab3_alt2[which(tab3_alt2$region==region),]
cor2 <- cor(tab3_alt2[,c(1,2,4,6,7,14:27)])
png(paste0(plot_descrip_dir,"/","corrplot","_",region,"_",Sys.Date(),".png"),width = 900,height = 900)   
corrplot::corrplot(cor2)
dev.off()
cor2 <- as.data.frame(as.matrix(cor2))
cor2$variable <- row.names(cor2)
cor2 <- cor2[,c(ncol(cor2),1:(ncol(cor2)-1))]
write.table(cor2,paste0(trans_results_dir,"/","correlations","_",region,"_",".csv"),quote=F,row.names = F,sep="|")

};rm(i)
################################
tab3_alt2 <-tab3_alt2_copy

################################3


#Fisher exact test and graphic

for(a in 1:length(regions)){
  region <- regions[[a]]
  tab3_alt2 <-tab3_alt2_copy
  
fish_table <- as.data.frame(matrix(ncol=6,nrow=6));
row.names(fish_table) <- colnames(tab3_alt2)[8:13];colnames(fish_table) <- colnames(tab3_alt2)[8:13];
set.seed(1234)
for(i in 8:13){
  
  for(j in 8:13){
    ii <- (i -7); jj <- (j -7);
    cat("row = ",i,"col = ",j,"\n")
    cat(" new row = ",ii," new col = ",jj,"\n")
    if(i==j) { fish_table[ii,jj] <- NA
    } else {
      
      fish_table[ii,jj] <- fisher.test(tab3_alt2[,i],tab3_alt2[,j],
                                       #workspace = 1234,
                                       # B=1000,
                                       alternative="two.sided",
                                       simulate.p.value=T
      )$p.value
    }
  };rm(j)
};rm(i)

fish_table_t <- fish_table
for(i in 1:ncol(fish_table_t)){
  for(j in 1:nrow (fish_table_t)){
    if(is.na(fish_table_t[j,i])){
      fish_table_t[j,i] <- NA
    }else if (fish_table_t[j,i] <=0.05){
      fish_table_t[j,i] <- 1
    } else {
      fish_table_t[j,i] <- 0
    }
  };rm(j)
};rm(i)

fish_table_t <- as.matrix(fish_table_t)

fish_table_t[lower.tri(fish_table_t)] <- NA
fish_table_t[lower.tri(fish_table_t)] <-t(fish_table_t)[lower.tri(t(fish_table_t))]
#as.numeric(t(fish_table_t[upper.tri(fish_table_t)]))

png(paste0(plot_descrip_dir,"/","heatmap_fisher","_",region,"_",Sys.Date(),".png"),width = 900,height = 600)   
heatmap(fish_table_t,na.rm=T,symm=T,col=c("gray","black"),keep.dendro=F,Rowv=NA)
dev.off()


fish_table$variable <- row.names(fish_table)
fish_table <- fish_table[,c(ncol(fish_table),1:((ncol(fish_table)-1)))]
write.table(fish_table,paste0(trans_results_dir,"/","fisher","_",region,"_",".csv"),quote=F,row.names = F,sep="|")
};rm(a)
################################

