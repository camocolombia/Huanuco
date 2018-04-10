require('readstata13')
#####

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
plot_descrip_dir <- paste0(plot_dir,"/","description");if(!file.exists(plot_descrip_dir)){dir.create(plot_descrip_dir)}###
#Defining Subset function
Subset <- function(df, pattern) {
  ind <- grepl(pattern=pattern, colnames(df))
  df[, ind]
}
#Calling dataset in STATA format
data <- as.data.frame(readstata13::read.dta13(paste0(dir,"/","Carasso_AgrobiodiversityDietPeru/Bases_600/MODULO_Q_600.dta"),
                                encoding = "UTF-8",
                                convert.factors=T)
)
#saving colnames information
write.csv(colnames(data),paste0(trans_process_dir,"/colnames.csv"),row.names=F,quote=F)

#defining Q1 module questions 
vars_q1 <- c(
"q03",
"q03_1",
"q04",
"q04_1",
"q04_2",
"q04_3",
"q04_4",
"q04_5",
"q04_6",
"q04_7",
"q04_8",
"q04_9",
"q05",
"q05_1",
"q05_2",
"q05_3",
"q06",
"q06_1",
"q06_2",
"q06_3",
"q06_4",
"q06_5",
"q06_6",
"q06_7",
"q06_8",
"q06_9",
"q06_10",
"q06_1_1",
"q07",
"q08",
"q09",
"q09_1",
"q09_2",
"q09_3",
"q09_4",
"q09_5",
"q09_6",
"q09_7",
"q09_8",
"q10",
"q10_1",
"q10_2",
"q10_3",
"q10_4",
"q10_5",
"q10_6",
"q10_7"
)

#defining Q2 module questions 
vars_q2<-c(
"q12",
"q12_1",
"q13",
"q13_1",
"q13_2",
"q13_3",
"q13_4",
"q13_5",
"q13_6",
"q13_7",
"q13_8",
"q13_9",
"q14",
"q14_1",
"q14_2",
"q14_3",
"q15",
"q15_1",
"q15_2",
"q15_3",
"q15_4",
"q15_5",
"q15_6",
"q15_7",
"q15_8",
"q15_9",
"q15_10",
"q15_1_1",
"q16");gc()

#defining basic information questions
info_vars<-c(
  "a00a_1",
  "a00a_2b",
  "aa",
  "a01",
  "a02",
  "a03",
  "a04",
  "a05",
  "a06",
  "a07",
  "a07_1",
  "a07_2",
  "a07_3",
  "ea",
  "e01",
  "e02",
  "ea2"
);gc()

##########################################################################################################################################
##########################################################################################################################################

#Gathering Q1 module table
q1 <- as.data.frame(matrix(ncol=length(vars_q1)+length(info_vars)+2,nrow=nrow(data)*22))

#basic information
for(j in 1:length(info_vars)){
  
  cat("Col: ",j,"of ",(ncol(q1)),"\n")

b <- lapply(1:nrow(data),function(i){
   
     b <- as.character(rep(data[i,j],22))

  return(b)
    })

b <- do.call(rbind,b)
b <- unlist(apply(b,1,as.list))
  
q1[,j] <- b

};rm(j)

##control question

c <- lapply(1:nrow(data),function(i){
  
  c <- as.character(rep(data[i,"q01"],22))
  
  return(c)
});c <- do.call(rbind,c);c <- unlist(apply(c,1,as.list))

q1[,c(18)] <- c


d <- lapply(1:nrow(data),function(i){
  
  d <- as.character(rep(data[i,"q02"],22))
  
  return(d)
});d <- do.call(rbind,d);d <- unlist(apply(d,1,as.list))

q1[,c(19)] <- d

rm(c,d)
# Module Q1 question
 for(j in 20:ncol(q1)){
   
        k <- j-19
      cat("Col: ",j,"of ",(ncol(q1))," | ",k,"\n")
      a <- (Subset(data,vars_q1[[k]]));
      a <-a[paste0(vars_q1[[k]],"_",c(paste0(0,1:9),10:22))];
      a <- do.call(rbind,apply(a,2,as.list))
      q1[,j] <- as.character(a)
      
    };rm(j)
colnames(q1)<-c(info_vars,"q01","q02",vars_q1)

#saving Q1 information

q1 <- q1[which(q1$q02=="Si"),]
write.table(q1,paste0(trans_process_dir,"/q1.csv"),row.names=F,quote=F,na="",sep="|")

##########################################################################################################################################
##########################################################################################################################################

#Gathering Q1 module table
q2 <- as.data.frame(matrix(ncol=length(vars_q2)+length(info_vars)+2,nrow=nrow(data)*13))

#basic information
for(j in 1:length(info_vars)){
  
  cat("Col: ",j,"of ",(ncol(q2)),"\n")
  
  b <- lapply(1:nrow(data),function(i){
    
    b <- as.character(rep(data[i,j],13))
    
    return(b)
  })
  
  b <- do.call(rbind,b)
  b <- unlist(apply(b,1,as.list))
  
  q2[,j] <- b
  
};rm(j)

##control question

c <- lapply(1:nrow(data),function(i){
  
  c <- as.character(rep(data[i,"q01"],13))
  
  return(c)
});c <- do.call(rbind,c);c <- unlist(apply(c,1,as.list))

q2[,c(18)] <- c


d <- lapply(1:nrow(data),function(i){
  
  d <- as.character(rep(data[i,"q11"],13))
  
  return(d)
});d <- do.call(rbind,d);d <- unlist(apply(d,1,as.list))

q2[,c(19)] <- d

rm(c,d)
# Module Q1 question
for(j in 20:ncol(q2)){
  
  k <- j-19
  cat("Col: ",j,"of ",(ncol(q2))," | ",k,"\n")
  a <- (Subset(data,vars_q2[[k]]));
  a <-a[paste0(vars_q2[[k]],"_",c(paste0(0,1:9),10:13))];
  a <- do.call(rbind,apply(a,2,as.list))
  q2[,j] <- as.character(a)
  
};rm(j)
colnames(q2)<-c(info_vars,"q01","q11",vars_q2)

q2 <- q2[which(q2$q11=="Si"),]
write.table(q2,paste0(trans_process_dir,"/q2.csv"),row.names=F,quote=F,na="",sep="|")

##########################################################################################################################################
##########################################################################################################################################
info_var_aux<-c(
  "a00a_1",
  "a00a_2b",
  "aa",
  "a01",
  "a02",
  "a03",
  "a04",
  "a05",
  "a06",
  "a07",
  "a07_1",
  "a07_2",
  "a07_3",
  "ea",
  "e01",
  "e02",
  "ea2",
  "f01",
  "h01",
  "i01",
  "j01",
  "k01",
  "k02",
  "l01",
  "l02",
  "m01",
  "m40",
  "m41",
  "m41_1",
  "m41_2",
  "m41_3",
  "m41_4",
  "m41_5",
  "m41_6",
  "m41_7",
  "m41_8",
  "m41_9",
  "m41_10",
  "m41_11",
  "m41_12",
  "m41_13",
  "m41_14",
  "m41_15",
  "m41_16",
  "m42",
  "m42_1",
  "m42_2",
  "m42_3",
  "m42_4",
  "m42_5",
  "m42_6",
  "m42_7",
  "m42_8",
  "m42_9",
  "m42_10",
  "m42_11",
  "m42_12",
  "m42_13",
  "m42_14",
  "n01",
  "o01",
  "p01",
  "p02",
  "q01",
  "q02",
  "q11"
  );gc()


#Gathering Q1 module table
aux <- as.data.frame(matrix(ncol=length(info_var_aux),nrow=nrow(data)))
colnames(aux) <- info_var_aux

aux <- data[,info_var_aux]
write.table(aux,paste0(trans_process_dir,"/auxiliar_vars.csv"),row.names=F,quote=F,na="",sep="|")
