#####
rm(list=ls())
library(AlgDesign)##optFedreov
library(gtools)##permutations packages
#####
data_6=as.matrix(permutations(6,6,v=1:6,repeats=F))
model_6=matrix(0,nrow=nrow(data_6),ncol=choose(ncol(data_6),2))
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
for(i in 1:nrow(data_6)){
  X=PWO(data_6[i,])
  model_6[i,]=X
}
design_6=optFederov(~.,model_6,criterion = "D",nTrials=120,nRepeats=1000)
model=cbind(1,design_6$design)
model=as.matrix(model)
MM=(1/nrow(model)*t(model)%*%model)
D=det(MM)^(1/ncol(MM))
#####D-optimal
m=ncol(data_6)
a=1
b=(1+(ncol(data_6)-2)*(1/3))
c=(1-2*(1/3))
D_optimal=(a*b^(m-1)*c^((m-1)*(m-2)/2))^(1/ncol(MM))
####D-efficiency
DE=D/D_optimal
DE
#####D_optimal^(1/ncol(Moment_matrix))