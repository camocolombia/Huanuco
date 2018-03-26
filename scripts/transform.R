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
dir  <- "D:/HUANUCO"
trans_dir <- paste0(dir,"/","csv");if(!file.exists(trans_dir)){dir.create(trans_dir)}
###
################################
Huanuco_csv <- read.csv(paste0(dir,"/","UM/Huanuco.csv"),header=T)

Huanuco_csv <- Huanuco_csv[,c("hhid","headsex","elcsa_cat","edu","assistance","nonaginc",
                              "hhsize",
                              "numchildren ", # new
                              "alt_mean","age","wealthindex","num_emp","inc","area",
                              "l_fields","m_fields","cd_field","cd_garden",
                              "cd_tree","cd_gardentree")]

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
  write.table(q1_to_join,paste0(trans_dir,"/","csv_to_join.csv"),quote=F,row.names = F,sep=",")
#Joining with Andy's data
  tab3 <- merge(q1_to_join,Huanuco_csv,by = "hhid")
row.names(tab3) <- tab3$hhid
tab3 <- tab3[,-1]


colnames(tab3)
factors_var <- c(1,2,10,11,12,13,14)
num_var <- c(3,4,5,6,7,8,9,15:27)


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

#variables with NAs
#c("alt_mean","wealthindex","inc")

tab3_alt <- tab3[,-c(2,16)]
for(i in 1:ncol(tab3_alt)){
  cat("Column: ",i," | ",length(tab3_alt[i][is.na(tab3_alt[,i]),]),"\n")
}
tab3_alt <- tab3_alt[complete.cases(tab3_alt),]
#saving table
write.table(tab3_alt,paste0(trans_dir,"/","table_q1_join.csv"),quote=F,row.names = T,sep=",")


ACP <- FactoMineR::PCA(tab3_alt,quali.sup = c(1,9,10,11,12,13),quanti.sup = 25,ncp=10,scale.unit = T,graph = T)
# plot(ACP)
# plot.PCA(ACP,choix = "var")
# hCPC <- HCPC(ACP,nb.clust = -1)
# plot.HCPC(hCPC,choice="3D.map")


tab3_alt2 <-tab3_alt

# for( i in c(1,9,10,11,12,13)){
#   tab3_alt2[,i] <- as.character(tab3_alt2[,i]) 
# }


tab3_alt2 <- tab3_alt2[,c(2:8,1,9:25)]
for(i in 1:ncol(tab3_alt2)){cat(class(tab3_alt2[,i])," | ",i,"\n")}

MFA_2 <- FactoMineR::MFA(base=tab3_alt2,
            # group = c(1,7,5,5,7),
            group = c(7,6,5,7),
           # type = c("n","c","n","c","c"),
           type = c("c","n","c","c"),
           
              # c("n",
              #   "c","c","c","c","c","c","c",
              #   "n","n","n","n","n",
              #   "c","c","c","c","c",
              #   "c","c","c","c","c","c","c"),
              
               ncp = 5,
            # name.group = c("Region",
            #                "Plants",
            #                "Socioeconomic",
            #                "Wealth",
            #                 "Farm variables")
           name.group = c("Plants",
                          "Socioeconomic",
                          "Wealth",
                           "Farm variables")
            #,num.group.sup=c(5)

)
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/
fviz_mfa_var(MFA_2, "group")
# Contribution to the first dimension
fviz_contrib(MFA_2, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(MFA_2, "group", axes = 2)
summary(MFA_2)
print(MFA_2)
MFA_MFA <- get_mfa(MFA_2,element = c("ind", "quanti.var", "quali.var", "group",
                                    "partial.axes")) #GET MFA
#plot(MFA_2,choix="var")
EIG_MFA <- get_eigenvalue(MFA_2) #GET EIGENVALUES
fviz_eig(MFA_2)
group <- get_mfa_var(MFA_2, "group")
##d
quanti.var <- get_mfa_var(MFA_2, "quanti.var")

fviz_mfa_var(MFA_2, "quanti.var", palette = "jco", 
             col.var.sup = "violet", repel = F,
             geom = c("point", "text"))

fviz_mfa_ind(MFA_2); fviz_mfa_var(MFA_2)#: Visualize the results for individuals and variables, respectively.

fviz_contrib(MFA_2, choice = "quanti.var", axes = 1, top = 20,
             palette = "jco")

fviz_contrib(MFA_2, choice = "quanti.var", axes = 2, top = 20,
             palette = "jco")

MFA_2$group$Lg

fviz_mfa_var(MFA_2, "quanti.var", col.var = "contrib", 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            col.var.sup = "violet", repel = TRUE,
            geom = c("point", "text"))

# Color by cos2 values: quality on the factor map
fviz_mfa_var(MFA_2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE)

ind <- get_mfa_ind(MFA_2)
ind

fviz_mfa_ind(MFA_2, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


#http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_mfa.html
fviz_mfa_ind(MFA_2, 
             habillage = c("region"), # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07","#000000"),
             addEllipses = TRUE, ellipse.type = "convex", geom="point",
             repel = TRUE # Avoid text overlapping
) 



fviz_mfa_quali_biplot(MFA_2, repel = FALSE, col.var = "#E7B800",
                      habillage = "region", addEllipses = TRUE, ellipse.level = 0.95)


fviz_mfa_ind(MFA_2, partial = "all") 



fviz_mfa_axes(MFA_2)

MFA_2$group$RV
MFA_2$group$Lg
MFA_2$inertia.ratio
MFA_2$group$contrib
MFA_2$group$cos2

dimdesc(MFA_2)


#http://factominer.free.fr/course/doc/MFA_course_slides.pdf


cor2 <- cor(tab3_alt[,-c(1,9,10,11,12,13,25)])
corrplot::corrplot(cor2)
write.table(cor2,paste0(trans_dir,"/","correlations.csv"),quote=F,row.names = T,sep=",")
#
dGower <- daisy(tab3_alt,metric = "gower")
summary(dGower)
h_C <- hclust(dGower,"ward.D")
plot(h_C)
coef(h_C)
coefHier(h_C) # ditto


z = agnes(dGower, diss = inherits(dGower, "dist"), metric = "euclidean",
stand = FALSE, method = "ward", par.method,
trace.lev = 0, keep.diss = TRUE)
plot(z,  main="plotit", which.plot = 1)



cord_Dist <- 1-cor2
h_cor <-  hclust(as.dist(cord_Dist),"ward.D")
plot(h_cor)


#write.table(data_to_analyze_q1,paste0(dir,"/","Q1_table.csv"),row.names = T,quote=F,sep="|",na="")

