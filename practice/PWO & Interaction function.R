####
rm(list=ls())
library(AlgDesign)##optFedreov
library(gtools)##permutations packages
#####
m=5 ##components
data=as.matrix(permutations(m,m,v=1:m,repeats=F))

#####PWO function
PWO=function(S){
  model=matrix(0,nrow=nrow(S),ncol=choose(ncol(S),2))
  for(k in 1:nrow(data)){
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

#####interaction function
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

#####run PWO function
model=PWO(data)
#####run inter fuction
model2=inter(data)

#####combine
model=cbind(1,model,model2)
model
#####Determine
MM=(1/nrow(data)*t(model)%*%model)
D=det(MM)^(1/ncol(MM))

#####opt
model=model[,-1]
design=optFederov(data=model,criterion = "D",nTrials=24,nRepeats=1000)
model=cbind(1,design$design)
model=as.matrix(model)
MM_O=(1/nrow(model)*t(model)%*%model)
D_O=det(MM_O)^(1/ncol(MM_O))
####D-efficiency
D_O/D

#####design-matrix
original=data[design$rows,]
original
#####save
#write.table(original,file="C:/Users/??pig/Desktop/design.txt",sep=" ",row.names=F,col.names=F)
