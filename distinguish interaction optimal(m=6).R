#####
rm(list=ls())
library(AlgDesign)##optFedreov
library(gtools)##permutations packages
#####load-data
#####according different m  to change file
data=read.table(file="C:/Users/stupi/Desktop/study/order of addition/Tsai_2021_JSPI/catalogue/optimal/6/optimal_6_24.txt",header=F)

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
  for(k in 1:(ncol(D)-1))
    for(kk in (k+1):ncol(D)){
      AA=as.matrix(D)
      for(i in 1:nrow(AA))
        for(j in 1:ncol(AA)){
          if(AA[i,j]==k){
            AA[i,j]=0
          }else if(AA[i,j]==kk){
            AA[i,j]=0
          }
        }
      BB=NULL
      for(i in 1:nrow(D)){
        BB=rbind(BB,AA[i,which(AA[i,]!=0)])
      }
      vv=c(1:ncol(D))
      vv=vv[c(-k,-kk)]
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

#####run function
dis1(data)
dis2(data)
