################################################################################################
################################################################################################
###Huanuco Survey data Script                                                                ###
################################################################################################
################################################################################################
#Calling R libraries

#install Java http://javadl.oracle.com/webapps/download/AutoDL?BundleId=230542_2f38c3b165be4555a1fa6e98c45e0808
require(xlsx)

################################
################################
#Workspace 
#####
dir  <- "D:/AICHI13/Huanuco"
trans_dir <- paste0(dir,"/","csv");if(!file.exists(trans_dir)){dir.create(trans_dir)}
trans_process_dir <- paste0(trans_dir,"/","processing");if(!file.exists(trans_process_dir)){dir.create(trans_process_dir)}

trans_results_dir <- paste0(trans_dir,"/","results");if(!file.exists(trans_results_dir)){dir.create(trans_results_dir)}

plot_dir <- paste0(dir,"/","plot");if(!file.exists(plot_dir)){dir.create(plot_dir)}
plot_cluster_dir <- paste0(plot_dir,"/","cluster");if(!file.exists(plot_cluster_dir)){dir.create(plot_cluster_dir)}
plot_full_dir <- paste0(plot_dir,"/","full");if(!file.exists(plot_full_dir)){dir.create(plot_full_dir)}
plot_reg_dir <- paste0(plot_dir,"/","region");if(!file.exists(plot_reg_dir)){dir.create(plot_reg_dir)}
plot_descrip_dir <- paste0(plot_dir,"/","description");if(!file.exists(plot_descrip_dir)){dir.create(plot_descrip_dir)}
#
###
csv_list_files <- list.files(trans_results_dir,pattern = ".csv$")
csv_list_names <- sub(".csv","",csv_list_files )
csv_files <-lapply(1:length(csv_list_files),function(i){
  x <- read.csv(paste0(trans_results_dir,"/",csv_list_files[[i]]),header = T,sep = "|")
  return(x)
})


lapply(1:length(csv_files),function(i){
  cat(i,"\n")
write.xlsx(x = csv_files[[i]], file = paste0(trans_results_dir,"/","SUMMARY.xls"),sheetName = csv_list_names[[i]], row.names = FALSE,append=T)
})
