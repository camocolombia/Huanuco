#Calling R libraries
require(factoextra);library(dendextend);require(corrplot);require(ggplot2)

################################
################################
#Workspace 
#####
dir  <- "D:/AICHI13/Huanuco"
trans_dir <- paste0(dir,"/","csv");if(!file.exists(trans_dir)){dir.create(trans_dir)}
trans_results_dir <- paste0(trans_dir,"/","results")
################################
#Reading RDS file to be used in clustering
#http://factominer.free.fr/course/doc/MFA_course_slides.pdf
tab3 <- readRDS(paste0(trans_results_dir,"/","firstStep.RDS"))


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

#for(i in 1:ncol(tab3_alt2)){cat(class(tab3_alt2[,i])," | ",i,"\n")} # print class per column

#Scaling numerical variables to calculate Gower distance
sc <- scale(tab3_alt2[,c(1:7,14:27)],scale = T)

tab3_alt2_alt <- cbind(sc[,c(1:7)],tab3_alt2[,8:13],sc[,c(8:21)])
#for(i in 1:ncol(tab3_alt2_alt)){cat(class(tab3_alt2_alt[,i])," | ",i,"\n")}  # print class per column

################################
#Calculating Gower distance for all data (categorical + numeric)

gow.mat <- cluster::daisy(tab3_alt2,"gower",stand=T) # using no scaled variables
gow.mat_sc <- cluster::daisy(tab3_alt2_alt,"gower",stand=F)
#identical(gow.mat,gow.mat_sc) #check if distances are similar each other

dissplot(as.dist(gow.mat), labels = NULL, method = "Spectral",  control = NULL, options = NULL)

dissplot(as.dist(gow.mat_sc), labels = NULL, method = "Spectral",  control = NULL, options = NULL)


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
maxNumb <- 50
#barplot(sort(ClusterEvents$height,decreasing=T)[1:maxNumb],names.arg=1:maxNumb,col="lightseagreen",main="Select the number of Cluster",xlab="Number of cluster",ylab="height") 
#Height barplot
barplot(sort(ClusterEvents_sc$height,decreasing=T)[1:maxNumb],names.arg=1:maxNumb,col="lightseagreen",main="Select the number of Cluster",xlab="Number of cluster",ylab="height") 

### Silhouette criteria
#https://stats.stackexchange.com/questions/302844/dissimilarity-matrix-number-of-cluster
#https://stats.stackexchange.com/questions/10540/how-to-interpret-mean-of-silhouette-plot

# plot(2:50, sapply(2:50, function(i) { 
#   cat(i,"\n")
#   mean(silhouette(cutree(hclust(d=gow.mat,method="ward.D"),k=i),dist=gow.mat)[,"sil_width"]) }),
#   xlab="Number of clusters", ylab="Average Silhouette", type="b", pch=20)

plot(2:50, sapply(2:50, function(i) { 
  cat(i,"\n")
  mean(silhouette(cutree(hclust(d=gow.mat_sc,method="ward.D"),k=i),dist=gow.mat_sc)[,"sil_width"]) }),
  xlab="Number of clusters", ylab="Average Silhouette", type="b", pch=20)


### Sum of square criteria (Elbow)
factoextra::fviz_nbclust(x=tab3_alt2_alt,FUN=hcut,diss=gow.mat_sc,k.max=50,method=c("wss"))#c("silhouette","wss","gap_stat")

################################
## saving results


#3 was optimal number of clusters
final_table <- tab3_alt2

ClusterEvents_sc      <- hclust(gow.mat,method="ward.D")
ClusterEvents_sc      <- hclust(gow.mat_sc,method="ward.D")

# Compute hierarchical clustering and cut into 3 clusters
#ClusterEvents_sc      <- hclust(gow.mat_sc,method="ward.D")
clust_per <- cutree(ClusterEvents_sc,h=sort(ClusterEvents_sc$height,decreasing = T)[3])

#res <- factoextra::hcut(x=gow.mat_sc, k = 3, stand = F,isdisss=T, hc_method = "ward.D",hc_func="hclust")
# Compute aglomerative coefficient for the hierarchical cluster
coef(res)
coefHier(res) # ditto

final_table$ID <- row.names(final_table)

final_table <- final_table[,c(ncol(final_table),2:(ncol(final_table)-1))]
#table(final_table$region,final_table$clust)
final_table$clust <- as.factor(as.character(res$cluster))
saveRDS(final_table,paste0(trans_dir,"/","cluster_table.RDS"))
################################
## Visualize results

         
# 
# factoextra::fviz_dend(res, rect = TRUE, cex = 0.5,
#          k_colors = c("#00AFBB","#FC4E07", "#E7B800" )) #"#FC4E07"


factoextra::fviz_dend(ClusterEvents_sc,k=3, rect = TRUE, cex = 0.5,
                      k_colors = c("#00AFBB","#FC4E07", "#E7B800" )) #"#FC4E07"
