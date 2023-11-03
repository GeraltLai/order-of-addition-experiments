#####
rm(list=ls())
library(AlgDesign)##optFedreov
library(gtools)##permutations packages
#####load-data
#####according different m  to change file
data=read.table(file="C:/Users/stupi/Desktop/study/order of addition/Tsai_2021_JSPI/catalogue/optimal/5/optimal_5_48.txt",header=F)

#####PWO function
PWO=function(S){
  model=matrix(0,nrow=nrow(S),ncol=choose(ncol(S),2))
  for(k in 1:nrow(S)){
    m=ncol(S)
    A=NULL
    for(x in 1:m) 
      for (i in 1:m) {
        if (S[k,i]==x)  
          A=c(A,i)
      }
    A1=NULL
    for(n in length(A))
      for(i in 1:(n-1))
        for(j in (i+1):n)
          if(A[i]>=A[j]){
            A1=c(A1,-1)
          }else{
            A1=c(A1,1)
          }
    X=A1
    model[k,]=X
  }
  return(model)
}

#####inter-function
inter=function(L){
  model2=matrix(0,nrow=nrow(L),ncol=2)
  for(l in 1:nrow(L)){
    i=1
    j=i+1
    k=j+1
    W=NULL
    if((which(L[l,]==i)<which(L[l,]==j)) & (which(L[l,]==i)<which(L[l,]==k)) & (which(L[l,]==j)>which(L[l,]==k)))
      W=c(W,-1/2,sqrt(3)/2)
    else if((which(L[l,]==i)>which(L[l,]==j)) & (which(L[l,]==i)<which(L[l,]==k)) & (which(L[l,]==j)<which(L[l,]==k)))
      W=c(W,-1/2,-(sqrt(3))/2)
    else if((which(L[l,]==i)>which(L[l,]==j)) & (which(L[l,]==i)>which(L[l,]==k)) & (which(L[l,]==j)<which(L[l,]==k)))
      W=c(W,-1/2,sqrt(3)/2)
    else if((which(L[l,]==i)<which(L[l,]==j)) & (which(L[l,]==i)>which(L[l,]==k)) & (which(L[l,]==j)>which(L[l,]==k)))
      W=c(W,-1/2,-(sqrt(3))/2)
    else 
      W=c(W,1,0)
    model2[l,1]=W[1]
    model2[l,2]=W[2]
  }
  return(model2)
}

#####distinguish interaction function-1
dis1=function(L){
  W=NULL
  for(l in 1:nrow(L)){
    i=1
    j=i+1
    k=j+1
    if((which(L[l,]==i)<which(L[l,]==j)) & (which(L[l,]==i)<which(L[l,]==k)) & (which(L[l,]==j)<which(L[l,]==k)))
      W=c(W,1)
    else if((which(L[l,]==i)<which(L[l,]==j)) & (which(L[l,]==i)<which(L[l,]==k)) & (which(L[l,]==j)>which(L[l,]==k)))
      W=c(W,2)
    else if((which(L[l,]==i)>which(L[l,]==j)) & (which(L[l,]==i)<which(L[l,]==k)) & (which(L[l,]==j)<which(L[l,]==k)))
      W=c(W,3)
    else if((which(L[l,]==i)>which(L[l,]==j)) & (which(L[l,]==i)>which(L[l,]==k)) & (which(L[l,]==j)<which(L[l,]==k)))
      W=c(W,4)
    else if((which(L[l,]==i)<which(L[l,]==j)) & (which(L[l,]==i)>which(L[l,]==k)) & (which(L[l,]==j)>which(L[l,]==k)))
      W=c(W,5)
    else if((which(L[l,]==i)>which(L[l,]==j)) & (which(L[l,]==i)>which(L[l,]==k)) & (which(L[l,]==j)>which(L[l,]==k)))
      W=c(W,6)
  }
  return(table(W))
}

#####distinguish interaction function-2
dis2=function(D){
  FF=NULL
  for(k in 1:choose(ncol(D),4)){
    AA=as.matrix(D)
    for(i in 1:nrow(AA))
      for(j in 1:ncol(AA)){
        if(AA[i,j]==k){
          AA[i,j]=0
        }
      }
    BB=NULL
    for(i in 1:nrow(D)){
      BB=rbind(BB,AA[i,which(AA[i,]!=0)])
    }
    vv=c(1:ncol(D))
    vv=vv[-k]
    DD=as.matrix(permutations(4,4,v=vv,repeats=F))
    EE=rep(0,nrow(DD))
    for(i in 1:nrow(DD))
      for(j in 1:nrow(D)){
        CC=abs(DD[i,]-BB[j,])
        if(sum(CC)==0)
          EE[i]=EE[i]+1
      }
  }
  return(EE)
}


#####
dis1(data)
dis2(data)
