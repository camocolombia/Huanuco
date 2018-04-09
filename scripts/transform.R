################################################################################################
################################################################################################
###Huanuco Survey data Script                                                                ###
################################################################################################
################################################################################################
#Calling R libraries
require(FactoMineR);require(factoextra);require(cluster);require(NbClust)

################################
################################
#Workspace 
#####
dir  <- "D:/AICHI13/Huanuco"
trans_dir <- paste0(dir,"/","csv");if(!file.exists(trans_dir)){dir.create(trans_dir)}
trans_process_dir <- paste0(trans_dir,"/","processing");if(!file.exists(trans_process_dir)){dir.create(trans_process_dir)}

trans_results_dir <- paste0(trans_dir,"/","results");if(!file.exists(trans_results_dir)){dir.create(trans_results_dir)}

plot_dir <- paste0(dir,"/","plot");if(!file.exists(plot_dir)){dir.create(plot_dir)}
plot_full_dir <- paste0(plot_dir,"/","full");if(!file.exists(plot_full_dir)){dir.create(plot_full_dir)}
plot_reg_dir <- paste0(plot_dir,"/","region");if(!file.exists(plot_reg_dir)){dir.create(plot_reg_dir)}

#
###
################################
Huanuco_csv <- read.csv(paste0(dir,"/","UM/Huanuco.csv"),header=T)

Huanuco_csv <- Huanuco_csv[,c("hhid","headsex","elcsa_cat","edu","assistance","nonaginc",
                              "hhsize","headage","numchildren","age","wealthindex","num_emp","inc",
                              "alt_mean","area","ch","per_sale","ced",
                              "l_fields","m_fields","cd_field","cd_garden","cd_tree","cd_gardentree")]

for(i in 2:6){
  Huanuco_csv[,i] <- as.character(Huanuco_csv[,i])
};rm(i)
            ####################
#Reading auxiliar vars
aux <- read.csv(paste0(dir,"/","csv/auxiliar_vars.csv"),header=T,na="",sep="|")

###Transforming Si and No options to 0 and 1

#choosing factor variables
#vars<-c(19,23,25:40,42:55,59,61,62)-3
vars_delete<-c(1,3,14,17,28,45) #delete variables
aux <- aux[-vars_delete]

vars <- c(19,23:53,57,59,60)
#transforming factors No and Si to 0 and 1
for( i in 1:length(vars)){
  i <- vars[[i]]
  cat(i,"\n")
  aux[,i] <- as.character(aux[,i])
  aux[,i][is.na(aux[,i])]<- 0
  aux[,i][aux[,i]=="No"]<- 0
  aux[,i][aux[,i]=="Si"] <- 1
};rm(i)


################################
################################
#Reading Q1 table
q1 <- read.csv(paste0(dir,"/csv/","q1.csv"),header=T,na="",sep="|")
q1 <- q1[-c(3,14,17,22,32,36,50,59)]
#choosing factor variables
#q1_vars <- c(3,5,7:15,17:19,21:32,34:43,44:51)
q1_vars <- c(16,18,19:58)
#  15,17:19,21:32,34:43,44:51)
#transforming factors No and Si to 0 and 1
for( i in 1:length(q1_vars)){
  i <- q1_vars[[i]]
  cat(i,"\n")
  q1[,i] <- as.character(q1[,i])
  q1[,i][is.na(q1[,i])]<- 0
  q1[,i][q1[,i]=="No"]<- 0
  q1[,i][q1[,i]=="Si"] <- 1
};rm(i)

###Organizing q06 question other responses.
q06_1_aux <- as.data.frame(matrix(ncol=3,nrow=nrow(q1)))
colnames(q06_1_aux) <- c("a00a_2b",paste0("q06_",11:12))
q06_1_aux[,1] <- q1[,2]

q1[which(q1["q06_1_1"]=="locro"),"q06_2"] <- 1 # locro to sancochado
q1[which(q1["q06_1_1"]=="refresco"),"q06_5"] <- 1 # refresco  to jugo
#q1[which(q1["q06_1_1"]!="locro"),"q06_2"] <- 0

q06_1_aux[2][which(q1[,"q06_1_1"]=="ensalada"),] <- 1

q06_1_aux[2][which(q1[,"q06_1_1"]=="ensalada, aji"),] <- 1
q06_1_aux[3][which(q1[,"q06_1_1"]=="ensalada, aji"),] <- 1
q06_1_aux[3][which(q1[,"q06_1_1"]=="aji"),] <- 1
#
q06_1_aux[2][which(q1[,"q06_1_1"]!="ensalada"),] <- 0
q06_1_aux[3][which(q1[,"q06_1_1"]!="aji"),] <- 0

###Organizing q07 question other responses.


q07_aux <- as.data.frame(matrix(ncol=4,nrow=nrow(q1)))
colnames(q07_aux) <- c("a00a_2b",paste0("q07_aux",1:3))
q07_aux[,1] <- q1[,2]


q07_aux[2][which(q1[,"q07"]=="Disminuido"),] <- 1
q07_aux[2][which(q1[,"q07"]!="Disminuido"),] <- 0


q07_aux[3][which(q1[,"q07"]=="Se mantiene igual"),] <- 1
q07_aux[3][which(q1[,"q07"]!="Se mantiene igual"),] <- 0

q07_aux[4][which(q1[,"q07"]=="Aumentado"),] <- 1
q07_aux[4][which(q1[,"q07"]!="Aumentado"),] <- 0
#q08

#unique(q1[,"q08"])

q1[which(q1["q08"]=="No sabe"),"q08"] <- NA
q1[which(q1["q08"]=="No aplica"),"q08"] <- NA
q1[which(q1["q08"]=="Se niega a responder"),"q08"] <- NA

q1_curated <- cbind(q1[,1:39],q06_1_aux[,2:3],q07_aux[,-1],q1[,43:58])

#Filtering by use
q1_curated <- q1_curated[which(q1_curated$q03_1==1),]
#Inspecting unique values per field
# for(i in 1:ncol(q1_curated)){
#   cat(unique(q1_curated[,i])," | ",i,"\n")
# }
q1_curated[,26][which(q1_curated[,26]=="SI")] <- 1
#Extracting unique households
ID <- unique(q1_curated$a00a_2b)
#Joining plants
for(i in 1:length(ID)){
  
  x <- q1_curated[which(q1_curated[,"a00a_2b"]== ID[[i]]),] #ID[[i]]
  x <- x[which(x[,"q03"]== "Jaramullaca" |
                 x[,"q03"]== "Mullaca"),] 
  
  if(nrow(x)==0){
    cat("ommiting ", ID[[i]]," | 0 column!","\n")
  } else if(nrow(x)==1) {
    cat("ommiting ", ID[[i]]," | 1 column!","\n")
  } else if(nrow(x)==2) {
    
    cat("processing ", ID[[i]]," | 2 columns!","\n")
    
    for(j in 18:ncol(x)){
      x[,j] <- as.numeric(as.character(x[,j]))
    };rm(j)
    
    x <- colSums(x[,18:ncol(x)],na.rm=T)
    for(k in 1:length(x)){
      if(x[k]>0){
        x[k] <- 1
      } else {
        x[k] <-0
      }
    };rm(k)

        x <- as.data.frame(t(x))
    
    for(j in 1:ncol(x)){
      x[,j] <- as.character(x[,j])
    };rm(j)
    
    #q1_curated[,18:ncol(q1_curated)][which(q1_curated[,"a00a_2b"]== ID[[i]] & q1_curated[,"q03"]== "Jaramullaca"),] <- NA
    q1_curated[,18:ncol(q1_curated)][which(q1_curated[,"a00a_2b"]== ID[[i]] & q1_curated[,"q03"]== "Jaramullaca"),] <- x
  }
  rm(x)
};rm(i)
#Joining plants
 #### 
for(i in 1:length(ID)){
  
  x <- q1_curated[which(q1_curated[,"a00a_2b"]== ID[[i]]),] #ID[[i]]
  x <- x[which(x[,"q03"]== "Monte purupuru" |
                 x[,"q03"]== "Puru Puru"),] 
  
  if(nrow(x)==0){
    cat("ommiting ", ID[[i]]," | 0 column!","\n")
  } else if(nrow(x)==1) {
    cat("ommiting ", ID[[i]]," | 1 column!","\n")
  } else if(nrow(x)==2) {
    
    cat("processing ", ID[[i]]," | 2 columns!","\n")
    
    for(j in 18:ncol(x)){
      x[,j] <- as.numeric(as.character(x[,j]))
    };rm(j)
    
    x <- colSums(x[,18:ncol(x)],na.rm=T)
    for(k in 1:length(x)){
      if(x[k]>0){
        x[k] <- 1
      } else {
        x[k] <-0
      }
    };rm(k)
    x <- as.data.frame(t(x))
    
    for(j in 1:ncol(x)){
      x[,j] <- as.character(x[,j])
    };rm(j)
    
    q1_curated[,18:ncol(q1_curated)][which(q1_curated[,"a00a_2b"]== ID[[i]] & q1_curated[,"q03"]== "Monte purupuru"),] <- x
    
  }
  rm(x)
};rm(i)
####
#table without duplicated plants

q1_curated$q03 <- as.character(q1_curated$q03)
q1_curated$q03[which(q1_curated[,"q03"]== "Jaramullaca")] <-  "Jaramullaca, Mullaca"
q1_curated$q03[which(q1_curated[,"q03"]== "Monte purupuru")] <-  "Monte purupuru, Puru Puru"

q1_curated <- q1_curated[which(q1_curated[,"q03"]!= "Mullaca"),]
q1_curated <- q1_curated[which(q1_curated[,"q03"]!= "Puru Puru"),]
q1_curated <- q1_curated[which(q1_curated[,"q03"]!= "Cushuro, cushuru"  &
                               q1_curated[,"q03"]!= "Callampa" &
                               q1_curated[,"q03"]!= "Monte carne" &
                               q1_curated[,"q03"]!= "Paku Paku" &  
                               q1_curated[,"q03"]!= "Tucllish"  
                              # q1_curated[,"q03"]!= "Yuyo, ñapus"
                                 ),]
#q1_curated[which(q1_curated[,"q03"]== "Mullaca"),]

for( i in 18:ncol(q1_curated)){
  cat(i,"\n")
  q1_curated[,i][is.na(q1_curated[,i])]<- 0
};rm(i)

#q1_curated <- q1_curated[,-c(31:44)]
################################
#Getting variables for analysis
###############################

#Tabulating variables
tabs <- lapply(19:60,function(i){
  cat(i,"\n")
  x <- as.data.frame.matrix(table(q1_curated[,2],q1_curated[,i],exclude=0))
  if(ncol(x)>0 & nrow(x) > 0){
  colnames(x) <- paste0(colnames(q1_curated)[i],"-",1)
  
  } else {
    cat("Skipping ",colnames(q1_curated)[i],"\n")
  #x <- NULL
    }
  return(x)
    })

#Extracting number of species
sp_tabs <- as.data.frame.matrix(table(q1_curated[,2],q1_curated[,17],exclude=0))
sp_tabs$sp_sum <- rowSums(sp_tabs,na.rm=T)
#sp_tabs$ID <- row.names(sp_tabs)
#sp_tabs <- sp_tabs[,c(17,1:16)];sp_tabs 

#Joining survey IDs
ID_tabs <- as.numeric(as.character(as.data.frame(table(q1_curated[,2],q1_curated[,19],exclude=0))[1][,1]))
tabs <- do.call(cbind, tabs)
tabs <- cbind(tabs,sp_tabs)
tabs$ID <- NA; tabs$ID <- as.integer(as.character(ID_tabs))
tabs <- tabs[,c(ncol(tabs),1:(ncol(tabs)-1))]
colnames(tabs)[1] <- "a00a_2b"
#Choosing auxiliar variables to use
aux <- aux[,c(1,8,9)]
aux[,1] <- as.integer(aux[,1])
#Joining to create a tabulate table at household level
tab2 <- merge(aux,tabs,"a00a_2b")

##species syummary table]]]


sp_summary <- lapply(42:56,function(i){
  cat(i,"\n")
  x <- as.data.frame.matrix(table(tab2$a07,tab2[,i],exclude=0))
  if(ncol(x)>0 & nrow(x) > 0){
    colnames(x) <- paste0(colnames(tab2)[i])
    
  } else {
    cat("Skipping ",colnames(tab2)[i],"\n")
    #x <- NULL
  }
  return(x)
})

sp_summary <- do.call(cbind,sp_summary)

write.table(t(sp_summary),paste0(trans_process_dir,"/","sp_summary_t.csv"),quote=F,row.names = T,sep="|")



#####Deriving variables to be used in the analysis
#Number of ecosystems (n_ecosystems)
  q04 <- tab2[,c(1,4:12)];
  for(i in 2:ncol(q04)){
    cat(i,"\n")
    q04[which(q04[,i]>1),i] <-1
  }
  q04$ecos <- NA; q04$ecos <- rowSums(q04[,2:10],na.rm=T)

#Number of agricultural practices (n_Agr_pract)
  q08_10 <- tab2[,c(1,30,35:41)]
  for(i in 2:ncol(q08_10)){
    cat(i,"\n")
    q08_10[which(q08_10[,i]>1),i] <-1
  }
  q08_10$nPractices <- NA;  q08_10$nPractices <- rowSums(q08_10[,2:9],na.rm=T)
#  Number of food preparations (n_preparations)
  q06 <-tab2[,c(1,16:25)];
  for(i in 2:ncol(q06)){
    cat(i,"\n")
    q06[which(q06[,i]>1),i] <-1
  }
  q06$nUses <- NA;  q06$nUses <- rowSums(q06[,2:10],na.rm=T)
  

#Joining in an unique table to be joined with Andy's variable
  q1_to_join <- as.data.frame(cbind(tab2[,c(1:3,57)],q04$ecos,tab2[,13:15],q06$nUses,q08_10$nPractices))
  colnames(q1_to_join) <- c("hhid","region","town","n_species","n_ecosystems","Sell","autoconsumption","O_uses",
                            "n_preparations","n_Agr_pract")
#saving table
  write.table(q1_to_join,paste0(trans_process_dir,"/","csv_to_join.csv"),quote=F,row.names = F,sep=",")
#Joining with Andy's data
  tab3 <- merge(q1_to_join,Huanuco_csv,by = "hhid")
row.names(tab3) <- tab3$hhid
tab3 <- tab3[,-1]



colnames(tab3)
factors_var <- c(1,2,10,11,12,13,14)
num_var <- c(3,4,5,6,7,8,9,15:32)


for(i in factors_var){
  tab3[,i] <-as.factor(as.character(tab3[,i]))
};rm(i)
#
for(i in num_var){
  tab3[,i] <-as.numeric(as.character(tab3[,i]))
};rm(i)

for(i in 1:ncol(tab3)){
  print(class(tab3[,i]))
}


#saving table
saveRDS(tab3,paste0(trans_results_dir,"/","firstStep.RDS"))
