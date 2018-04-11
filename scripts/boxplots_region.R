#Calling R libraries
require(FactoMineR);require(factoextra);require(cluster);require(NbClust);library(dendextend)

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
#
################################
#Reading RDS file to be used in clustering
#http://factominer.free.fr/course/doc/MFA_course_slides.pdf
tab3 <- readRDS(paste0(trans_results_dir,"/","firstStep.RDS"))
final_table <- readRDS(paste0(trans_results_dir,"/","cluster_table_NO_REGION.RDS"))

region <- readRDS(paste0(trans_results_dir,"/","cluster_table.RDS"))$region

################################
#Ommiting variables with NAs
#c("alt_mean","wealthindex","inc","per_sale")
################################

tab3_alt <- tab3[,-c(2,19,21,22,25)]
for(i in 1:ncol(tab3_alt)){
  cat("Column: ",i," | ",length(tab3_alt[i][is.na(tab3_alt[,i]),]),"\n")
}
tab3_alt <- tab3_alt[complete.cases(tab3_alt),]


################################
tab3_alt2 <- tab3_alt[,c(2:8,1,9:27)]
tab3_alt2 <-tab3_alt[-c(8)]


black.bold.italic.16.text <- element_text(face = "italic", color = "black", size = 30)

numerical <- tab3_alt2[,c(2,3,5,7,13:26)]
numerical$cluster <- NA; numerical$cluster <- final_table$clust
numerical$region <- NA;numerical$region  <- region
for( i in 1:(ncol(numerical)-2)){
  cat(i,"\n")
   ########
  prom1<-ggplot(numerical, aes(x =region, y =numerical[,i] ,fill=region)) +
    geom_boxplot()+
    #  stat_boxplot(geom ='errorbar') +
    #scale_fill_manual(labels=levels(taxa_sub$Dormancy_cat),values = unique(taxa_sub$color))+
    #scale_colour_manual(labels =unique(taxa_sub$Dormancy_cat),values = unique(taxa_sub$color))+
    #c("#0035ff","#00ff19","#faff00","#e60b00")
    # scale_fill_manual(name="region\n",labels = levels(taxa_sub$Dormancy_cat),values = c("#799244","#c3d69b","#f2dcdb","#d99694"))+
    xlab("Region") +
    ylab(colnames(numerical)[i])+  
    #ggtitle(NAME) + 
    #scale_shape_discrete(name="",label=c("Very High","High","Low","Very low"))+
    #theme(panel.background = element_rect(fill = "gray95"),text=element_text(size=42),axis.text.x  = element_text(size=42,colour="black"),axis.text.y  = element_text(size=42,colour="black"),legend.position="none")+ 
    theme(panel.background = element_rect(fill = "gray90"),text=element_text(size=36),axis.text.x  =black.bold.italic.16.text,axis.text.y  = element_text(size=30,colour="black"),legend.title=element_text(size=36,colour="black"))
  
  ggsave(paste0(plot_reg_dir,"/",as.character(colnames(numerical)[i]),"_REGION_",Sys.Date(),".pdf"),prom1, units="in",width=20,height=9,scale=2,dpi=600)
  
  
}
