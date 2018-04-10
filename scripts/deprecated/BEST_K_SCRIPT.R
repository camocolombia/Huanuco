
#############################
##Index Similarity Clusters##
#############################

indexsim=function(X,Y,k){
delta=matrix(0,k,k)
for(i in 1:k){
for(j in 1:k){
delta[i,j]=(2*sum(Y[X[]==i]==j))/(sum(Y[]==j)+sum(X[]==i))
}}
return(sum(apply(delta,1,max))/k)}

##########################
##Index Quality Clusters##
##########################

indexqual=function(data,cluster,k){
  
c=sum(sqrt(apply((data-apply(data,1,mean))^2,1,sum)))
center=matrix(0,k,dim(data)[2])  
E=array(0,k)
for(i in 1:k){
if(sum(cluster[]==i)==1){ center[i,]=data[cluster[]==i,] 
                          E[i]=0}else{
center[i,]=apply(data[cluster[]==i,],2,mean)
d=array(0,sum(cluster[]==i))
for(j in 1:sum(cluster[]==i)){
d[j]=sqrt(sum((data[cluster[]==i,][j,]-center[i,])^2))}
E[i]=sum(d)} }

D=matrix(0,k,k)
for(j in 1:k){
for(i in 1:k){
D[i,j]=sqrt(sum((center[j,]-center[i,])^2))}}
return(((1/k)*(c/sum(E))*(max(D)))^2)}

###############
##Index Elbow##
###############

elbow=function(data,cluster,k){
g=apply(data,2,mean)
total=sum((data-g)^2)
entre=array(0,k)
for(i in 1:k){
if(sum(cluster[]==i)==1){entre[i]=sum((data[cluster[]==i,]-g)^2)}
else{entre[i]=sum((apply(data[cluster[]==i,],2,mean)-g)^2)*sum(cluster[]==i)}}
entre=sum(entre)
return(entre/total)}

###############
##Runs best k##
###############





bestk=function(data,ini,iter,kmax,type,dist.met){
  
#   data=x
#   iter=100
# kmax=20  
# j=1
library(cclust)
library(vegan) 
simil=matrix(0,iter,kmax-1)
qual=matrix(0,iter,kmax-1)
elbow=matrix(0,iter,kmax-1)
for(j in ini:kmax){
indexs=matrix(0,iter,3)
for(i in 1:iter){
  
  #print(c(i,j))
  cat("testing ",i, " iterations and ",j," clusters","\n")
  
  
  if(type=="hierarchical"){
    
    if(dist.met=="mahalanobis"){
      

      cat("hierarchical cluster approach"," Mahalanobis distance used","\n")  #X <-cclust(data,j,100,verbose=FALSE,method="kmeans")
      
   
      
X<-hclust(vegdist(data,method="mahalanobis"),method = "ward.D")#NEW
X2<-cutree(X,k=j)#h=10#NEW
X$cluster<-as.numeric(X2)#NEW
X$size<-as.numeric(tapply(X$cluster,X$cluster,length))
#Y <-cclust(data,j,100,verbose=FALSE,method="kmeans")
 
Y<-hclust(vegdist(data,method="mahalanobis"),method = "ward.D")#NEWY2<-cutree(Y,k=j)#h=10#NEW
Y2<-cutree(Y,k=j)#h=10#NEW
Y$cluster<-as.numeric(Y2)#NEW
Y$size<-as.numeric(tapply(Y$cluster,Y$cluster,length))#NEW


    }else{
      cat("hierarchical cluster approach"," Euclidean distance used","\n")
      
      X<-hclust(dist(data,method = "euclidean"),method = "ward.D")#NEW
      X2<-cutree(X,k=j)#h=10#NEW
      X$cluster<-as.numeric(X2)#NEW
      X$size<-as.numeric(tapply(X$cluster,X$cluster,length))
      #Y <-cclust(data,j,100,verbose=FALSE,method="kmeans")
      
      Y<-hclust(dist(data,method = "euclidean"),method = "ward.D")#NEW
      Y2<-cutree(Y,k=j)#h=10#NEW
      Y$cluster<-as.numeric(Y2)#NEW
      Y$size<-as.numeric(tapply(Y$cluster,Y$cluster,length))#NEW      
      
      
    }  
} else{
    cat("Non hierarchical cluster approach","\n")
    
  X <-cclust(data,j,100,verbose=FALSE,method="kmeans")
  Y <-cclust(data,j,100,verbose=FALSE,method="kmeans")
  
    
  }
  
if(min(X$size)==0){indexs[i,]=c(0,0,0)} else{
indexs[i,]=c(indexsim(X$cluster,Y$cluster,j),indexqual(data,X$cluster,j),elbow(data,X$cluster,j))}

if(indexs[i,2]==indexs[1,2]){clustqual = X$cluster}
else{ if(indexs[i,2]>indexs[i-1,2]){clustqual = X$cluster}
else{clustqual = clustqual}}
                 
if(indexs[i,3]==indexs[1,3]){clustelbow = X$cluster}
else{ if(indexs[i,3]>indexs[i-1,3]){clustelbow = X$cluster}
else{clustelbow = clustelbow}}}

if(j==2){clustqualf=clustqual} else {clustqualf=cbind(clustqualf,clustqual)}
if(j==2){clustelbowf=clustelbow} else {clustelbowf=cbind(clustelbowf,clustelbow)}

simil[,j-1]=indexs[,1]
qual[,j-1]=indexs[,2]
elbow[,j-1]=indexs[,3]}

results=list()
results[[1]]=data.frame(simil)
results[[2]]=data.frame(qual)
results[[3]]=data.frame(elbow)
results[[4]]=data.frame(clustqualf)
results[[5]]=data.frame(clustelbowf)
names(results[[1]])=paste("k:",2:kmax)
names(results[[2]])=paste("k:",2:kmax)
names(results[[3]])=paste("k:",2:kmax)
names(results[[4]])=paste("k:",2:kmax)
names(results[[5]])=paste("k:",2:kmax)

return(results)}




