#####
rm(list=ls())
library(AlgDesign)##optFedreov
library(gtools)##permutations packages
#####
data_5=as.matrix(permutations(5,5,v=1:5,repeats=F))
model_5=matrix(0,nrow=nrow(data_5),ncol=choose(ncol(data_5),2))
PWO=function(S){
  m=length(S)
  A=NULL
  for(x in 1:m) 
    for (i in 1:m) {
      if (S[i]==x)  
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
  return(A1)
}
for(i in 1:nrow(data_5)){
  X=PWO(data_5[i,])
  model_5[i,]=X
}
design_5=optFederov(~.,model_5,criterion = "D",nTrials=120,nRepeats=1000)
model=cbind(1,design_5$design)
model=as.matrix(model)
MM=(1/nrow(model)*t(model)%*%model)
D=det(MM)^(1/ncol(MM))
#####D-optimal
m=ncol(data_5)
a=1
b=(1+(ncol(data_5)-2)*(1/3))
c=(1-2*(1/3))
D_optimal=(a*b^(m-1)*c^((m-1)*(m-2)/2))^(1/ncol(MM))
#####D-efficiency
DE=D/D_optimal
DE

