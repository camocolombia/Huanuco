#Calling R libraries
require(factoextra);library(dendextend);require(corrplot);require(ggplot2);library(seriation);require(cluster)

################################
################################
#Workspace 
#####
dir  <- "D:/AICHI13/Huanuco"
trans_dir <- paste0(dir,"/","csv");if(!file.exists(trans_dir)){dir.create(trans_dir)}
trans_dir <- paste0(dir,"/","csv");if(!file.exists(trans_dir)){dir.create(trans_dir)}
trans_process_dir <- paste0(trans_dir,"/","processing");if(!file.exists(trans_process_dir)){dir.create(trans_process_dir)}

trans_results_dir <- paste0(trans_dir,"/","results");if(!file.exists(trans_results_dir)){dir.create(trans_results_dir)}

plot_dir <- paste0(dir,"/","plot");if(!file.exists(plot_dir)){dir.create(plot_dir)}
plot_cluster_dir <- paste0(plot_dir,"/","cluster");if(!file.exists(plot_cluster_dir)){dir.create(plot_cluster_dir)}
plot_full_dir <- paste0(plot_dir,"/","full");if(!file.exists(plot_full_dir)){dir.create(plot_full_dir)}
plot_reg_dir <- paste0(plot_dir,"/","region");if(!file.exists(plot_reg_dir)){dir.create(plot_reg_dir)}
plot_descrip_dir <- paste0(plot_dir,"/","description");if(!file.exists(plot_descrip_dir)){dir.create(plot_descrip_dir)}
################################
#Reading RDS file to be used in clustering
#http://factominer.free.fr/course/doc/MFA_course_slides.pdf
tab3 <- readRDS(paste0(trans_results_dir,"/","firstStep.RDS"))

#https://stat.ethz.ch/education/semesters/ss2012/ams/slides/v4.2.pdf
################################
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

tab3_alt2[,9] <- sapply(tab3_alt2[,9], FUN=function(x) ifelse(x==1, TRUE, FALSE))
tab3_alt2[,12] <- sapply(tab3_alt2[,12], FUN=function(x) ifelse(x==1, TRUE, FALSE))
tab3_alt2[,13] <- sapply(tab3_alt2[,13], FUN=function(x) ifelse(x==1, TRUE, FALSE))

tab3_alt2 <- tab3_alt2[,-c(3,5)]

#for(i in 1:ncol(tab3_alt2)){cat(class(tab3_alt2[,i])," | ",i,"\n")} # print class per column

#Scaling numerical variables to calculate Gower distance
sc <- scale(tab3_alt2[,c(1:5,12:25)],scale = T,center=F)#tab3_alt2[,c(1:7,14:27)]

tab3_alt2_alt <- cbind(sc[,c(1:5)],tab3_alt2[,6:11],sc[,c(6:19)])#sc[,c(1:7)],tab3_alt2[,8:13],sc[,c(8:21)]
#tab3_alt2_alt <- tab3_alt2_alt[,-c(3,5)]
#9,12, 13 are logical

# for(i in 8:13){
#   cat(unique(tab3_alt2[,i]),"\n")
# print(table(tab3_alt2[,8],tab3_alt2[,i]))
# }
#for(i in 1:ncol(tab3_alt2_alt)){cat(class(tab3_alt2_alt[,i])," | ",i,"\n")}  # print class per column

################################
#Calculating Gower distance for all data (categorical + numeric)

#https://stats.stackexchange.com/questions/123624/gower-distance-with-r-functions-gower-dist-and-daisy?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

gow.mat <- cluster::daisy(tab3_alt2,"gower",stand=T) # using no scaled variables
gow.mat_sc <- cluster::daisy(tab3_alt2_alt,"gower",stand=F)
#identical(gow.mat,gow.mat_sc) #check if distances are similar each other

###Exploring dissimilarity
#dissplot(as.dist(gow.mat), labels = NULL, method = "Spectral",  control = NULL, options = NULL)

#dissplot(as.dist(gow.mat_sc), labels = NULL, method = "Spectral",  control = NULL, options = NULL)


################################
## Comparing HCLUST and AGNES 
# agnes_res <- cluster::agnes(x=gow.mat_sc,diss=T,method="ward",stand=F)
# hclust_res <- hclust(gow.mat_sc,method="ward.D")
# par(mfrow=c(1,2))
# plot(as.hclust(agnes_res));rect.hclust(agnes_res,k=3)
# plot(hclust_res);rect.hclust(hclust_res,k=3)
# 
# #http://www.sthda.com/english/wiki/print.php?id=237
# dend1 <- as.dendrogram (agnes_res)
# dend2 <- as.dendrogram (hclust_res)
# 
# dend_list <- dendlist(dend1, dend2)
# tanglegram(dend1, dend2,
#            highlight_distinct_edges = FALSE, # Turn-off dashed lines
#            common_subtrees_color_lines = FALSE, # Turn-off line colors
#            common_subtrees_color_branches = TRUE, # Color common branches 
#            main = paste("entanglement =", round(entanglement(dend_list), 2))
# )
# 
# 
# # Cophenetic correlation matrix
# cor.dendlist(dend_list, method = "cophenetic")
# corrplot::corrplot(cor.dendlist(dend_list, method = "cophenetic"))


#fviz_nbclust(x=tab3_alt2,FUN=hcut,diss=gow.mat,k.max=50,method=c("silhouette"))#c("silhouette","wss","gap_stat")


################################
## Defining Optimal number of clusters



###Height criteria


#ClusterEvents      <- hclust(gow.mat,method="ward.D")
ClusterEvents_sc      <- hclust(gow.mat_sc,method="ward.D")
maxNumb <- 20

#Height barplot

#barplot(sort(ClusterEvents$height,decreasing=T)[1:maxNumb],names.arg=1:maxNumb,col="lightseagreen",main="Select the number of Cluster",xlab="Number of cluster",ylab="height") 


png(paste0(plot_cluster_dir,"/","barplotGraph","_",Sys.Date(),".png"),width = 900,height = 600)   


barplot(sort(ClusterEvents_sc$height,decreasing=T)[1:maxNumb],names.arg=1:maxNumb,col="lightseagreen",main="Select the number of Cluster",xlab="Number of cluster",ylab="height") 
abline(h=0)  
dev.off()


### Silhouette criteria
#https://stats.stackexchange.com/questions/302844/dissimilarity-matrix-number-of-cluster
#https://stats.stackexchange.com/questions/10540/how-to-interpret-mean-of-silhouette-plot

# plot(2:50, sapply(2:50, function(i) { 
#   cat(i,"\n")
#   mean(silhouette(cutree(hclust(d=gow.mat,method="ward.D"),k=i),dist=gow.mat)[,"sil_width"]) }),
#   xlab="Number of clusters", ylab="Average Silhouette", type="b", pch=20)


png(paste0(plot_cluster_dir,"/","silhouette","_",Sys.Date(),".png"),width = 900,height = 600)   

plot(2:50, sapply(2:50, function(i) { 
  cat(i,"\n")
  mean(silhouette(cutree(hclust(d=gow.mat_sc,method="ward.D"),k=i),dist=gow.mat_sc)[,"sil_width"]) }),
  xlab="Number of clusters", ylab="Average Silhouette", type="b", pch=20,xlim = c(1,50))
xtick<-seq(1, 50, by=1)
axis(side=1, at=xtick, labels = TRUE)
abline(h=0)  
dev.off()

### Sum of square criteria (Elbow)
wss_plot <- factoextra::fviz_nbclust(x=tab3_alt2_alt,FUN=hcut,diss=gow.mat_sc,k.max=50,method=c("wss"))#c("silhouette","wss","gap_stat")
#factoextra::fviz_nbclust(x=tab3_alt2_alt,FUN=hcut,diss=gow.mat_sc,k.max=50,method=c("silhouette"))#c("silhouette","wss","gap_stat")
ggsave(paste0(plot_cluster_dir,"/","WSS_plot","_",Sys.Date(),".png"),wss_plot, units="in",width=20,height=9,scale=1,dpi=600)

################################
## saving results


#3 was optimal number of clusters
final_table <- tab3_alt2

# Compute hierarchical clustering and cut into 3 clusters
#ClusterEvents_sc      <- hclust(gow.mat_sc,method="ward.D")
clust_per <- cutree(ClusterEvents_sc,h=sort(ClusterEvents_sc$height,decreasing = T)[3])

#res <- factoextra::hcut(x=gow.mat_sc, k = 3, stand = F,isdisss=T, hc_method = "ward.D",hc_func="hclust")
# Compute aglomerative coefficient for the hierarchical cluster
coef(ClusterEvents_sc)
coefHier(ClusterEvents_sc) # ditto

final_table$ID <- row.names(final_table)

final_table <- final_table[,c(ncol(final_table),1:(ncol(final_table)-1))]
#table(final_table$region,final_table$clust)
final_table$clust <- as.factor(as.character(clust_per))
tapply(final_table$clust,final_table$clust,length)
saveRDS(final_table,paste0(trans_results_dir,"/","cluster_table.RDS"))
write.table(final_table,paste0(trans_results_dir,"/","cluster_table.csv"),row.names=F,sep="|")
################################
## Visualize results

         
# 
# factoextra::fviz_dend(res, rect = TRUE, cex = 0.5,
#          k_colors = c("#00AFBB","#FC4E07", "#E7B800" )) #"#FC4E07"

#http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_dend.html
dend_plot <- factoextra::fviz_dend(ClusterEvents_sc,k=3, rect = TRUE, cex = 0.5,
                      k_colors = c("blue", "green3", "red")
                      #k_colors = c("#00AFBB","#FC4E07", "#E7B800" )
                      ) #"#FC4E07"
ggsave(paste0(plot_cluster_dir,"/","dendogram","_",Sys.Date(),".png"),dend_plot, units="in",width=30,height=9,scale=1,dpi=600)


dend_final <- list(tree=ClusterEvents_sc,clust=clust_per)
saveRDS(dend_final,paste0(trans_results_dir,"/","cluster_tree.RDS"))
