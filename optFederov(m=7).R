#####
rm(list=ls())
library(AlgDesign)##optFedreov
library(gtools)##permutations packages
#####
data_7=as.matrix(permutations(7,7,v=1:7,repeats=F))
model_7=matrix(0,nrow=nrow(data_7),ncol=choose(ncol(data_7),2))
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
for(i in 1:nrow(data_7)){
  X=PWO(data_7[i,])
  model_7[i,]=X
}
design_7=optFederov(~.,model_7,criterion = "D",nTrials=84,nRepeats=100)

model=cbind(1,design_7$design)
model=as.matrix(model)
MM=(1/nrow(model)*t(model)%*%model)
D=det(MM)^(1/ncol(MM))
#####D-optimal
m=ncol(data_7)
a=1
b=(1+(ncol(data_7)-2)*(1/3))
c=(1-2*(1/3))
D_optimal=(a*b^(m-1)*c^((m-1)*(m-2)/2))^(1/ncol(MM))
####D-efficiency
DE=D/D_optimal
DE
#####D_optimal^(1/ncol(Moment_matrix))
