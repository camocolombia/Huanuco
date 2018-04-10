################################################################################################
################################################################################################
###Huanuco Survey data Script                                                                ###
################################################################################################
################################################################################################
#Calling R libraries
require(FactoMineR);require(factoextra);require(cluster)

################################
################################
#Workspace 
dir <- "D:/HUANUCO"
################################
################################
#Reading auxiliar vars
aux <- read.csv(paste0(dir,"/","auxiliar_vars.csv"),header=T,na="",sep="|")

###Transforming Si and No options to 0 and 1

#choosing factor variables
vars<-c(19,23,25:40,42:55,59,61,62)-3


for( i in 1:length(vars)){
  i <- vars[[i]]
  cat(i,"\n")
  aux[,i] <- as.character(aux[,i])
  aux[,i][aux[,i]=="No"]<- 0
  aux[,i][aux[,i]=="Si"] <- 1
};rm(i)


################################
################################
#Reading Q1 table
q1 <- read.csv(paste0(dir,"/","q1.csv"),header=T,na="",sep="|")
#choosing factor variables
q1_vars <- c(3,5,7:15,17:19,21:32,34:43,44:51)

for( i in 1:length(q1_vars)){
  i <- q1_vars[[i]]
  cat(i,"\n")
  q1[,i] <- as.character(q1[,i])
  q1[,i][q1[,i]=="No"]<- 0
  q1[,i][q1[,i]=="Si"] <- 1
};rm(i)


################################
###############################
#Joining tables to be analyzed Q1
a_q <- merge(aux,q1)
a_q <- a_q[,c(colnames(aux),colnames(q1))]
#Saving temporal table
#write.table(a_q,paste0(dir,"/","q1_v.csv"),row.names = F,quote=F,sep="|",na="")

#Filtering Q1 table with values
a_q <- a_q[which(a_q[,64]!=0),]

#transforming data to use
for( i in c(89:91,94:102)){
  cat(i,"\n")
  a_q[,i][is.na(a_q[,i])]<- 0
};rm(i)

################################################################
################################################################

# removing data of Q1
a_q_v2 <- a_q[,-c(1,2,3,4,5,6,7,8,9:62,64,65,75,79,94,103)]
#a_q2_2 <- a_q2[,-c(1,2,3,4,5,6,7,8,9:62,64,65,75:110)]

a_q_v2[,26][a_q_v2[,26]=="No aplica"] <- NA
a_q_v2[,26][a_q_v2[,26]=="No sabe"] <- NA
a_q_v2[,26][a_q_v2[,26]=="Se niega a responder"] <- NA
a_q_v2[,26] <- factor(a_q_v2[,26])
a_q_v2[,27][a_q_v2[,27]=="No aplica"] <- NA
a_q_v2[,27][a_q_v2[,27]=="No sabe"] <- NA
a_q_v2[,27][a_q_v2[,27]=="Se niega a responder"] <- NA
a_q_v2[,27] <- factor(a_q_v2[,27])

#Getting frequencies from each variable for Q1
data_to_analyze_q1 <- lapply(1:ncol(a_q_v2),function(i){
  cat(i,"\n")
  a <- as.data.frame.matrix(table(a_q[,1],a_q_v2[,i]))
  
  if(dim(a)[2]>2){
    a <- as.data.frame(a)
    } else if(dim(a)[2]==2) {
    cat(colnames(a_q_v2)[i],"\n")
    a <- as.data.frame(a[,2])
    colnames(a) <- colnames(a_q_v2)[i]
  } else if(dim(a)[2]==1){
    cat(colnames(a_q_v2)[i],"\n")
    a <- as.data.frame(a)
    colnames(a) <- colnames(a_q_v2)[i]
  }
  return(a)
  })

data_to_analyze_q1 <- do.call(cbind, data_to_analyze_q1)
colnames(data_to_analyze_q1)[1:22] <- paste0("Planta: ",colnames(data_to_analyze_q1)[1:22])
colnames(data_to_analyze_q1)[23:31] <- paste0("Sitio: ",colnames(data_to_analyze_q1)[23:31])
colnames(data_to_analyze_q1)[32:46] <- paste0("Tipo de consumo: ",colnames(data_to_analyze_q1)[32:46])
colnames(data_to_analyze_q1)[47:49] <- paste0("Cantidad colectada: ",colnames(data_to_analyze_q1)[47:49])
colnames(data_to_analyze_q1)[51:58] <- paste0("Fuente de semilla: ",colnames(data_to_analyze_q1)[51:58])
colnames(data_to_analyze_q1)[59:65] <- paste0("Practicas: ",colnames(data_to_analyze_q1)[59:65])

# getting number of collected species

data_to_analyze_q1$Sp_richness <- as.numeric(rowSums(data_to_analyze_q1[,c(1:22)]))

#ommiting columns in 0
data_to_analyze_q1 <- data_to_analyze_q1[,(colSums(data_to_analyze_q1,na.rm = T)>1)]
data_to_analyze_q1$ID <- row.names(data_to_analyze_q1)
#info to be added
info_aux <-  cbind(aux[,c(1,4,5,6,8)])
colnames(info_aux) <- c("ID","lon","lat","altura","Zona")

ss <- merge(data_to_analyze_q1,info_aux,by = "ID")
ss <- ss[,c(colnames(info_aux),colnames(data_to_analyze_q1))]
ss <- ss[,-ncol(ss)]
row.names(ss) <-ss$ID
ss <- ss[,-1]
data_to_analyze_q1 <-ss; rm(ss)

#plot(data_to_analyze_q1[,c(1,2)],col=data_to_analyze_q1[,4],pch=16)

#PCA
sACP <- FactoMineR::PCA(data_to_analyze_q1[,-c(1:2,5:26)],quali.sup = 2,ncp=18,scale.unit = T,graph = F)
summary.PCA(sACP)
plot.PCA(sACP)
#HCPC
hCPC <- HCPC(sACP,nb.clust = -1)
plot.HCPC(hCPC,choice="map")
#Cluster
data_to_analyze_q1_dist <- dist(scale(data_to_analyze_q1[,-c(1,2,4)],center = T,scale = T),"euclidean")
h_q1 <-  hclust(data_to_analyze_q1_dist,"ward.D");dCTree <- cutree(h_q1,3)
plot(h_q1)
#Saving data table for Q1
write.table(data_to_analyze_q1,paste0(dir,"/","Q1_table.csv"),row.names = T,quote=F,sep="|",na="")


#plot(data_to_analyze_q1$altura,data_to_analyze_q1$Sp_richness)


cor_A <- cor(data_to_analyze_q1[,-c(1,2,4,5:26)])
write.table(cor_A,paste0(dir,"/","cor_q1.csv"),row.names = T,quote=F,sep="|",na="")

cord_Dist <- 1-cor_A
h_cor <-  hclust(as.dist(cord_Dist),"ward.D")
plot(h_cor)
