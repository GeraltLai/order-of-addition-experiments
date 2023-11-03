#####
rm(list=ls())
library(AlgDesign)##optFedreov
library(gtools)##permutations packages
#####
data_4=as.matrix(permutations(4,4,v=1:4,repeats=F))
model_4=matrix(0,nrow=nrow(data_4),ncol=choose(ncol(data_4),2))
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
for(i in 1:nrow(data_4)){
  X=PWO(data_4[i,])
  model_4[i,]=X
}
design_4=optFederov(~.,model_4,criterion = "D",nTrials=12)
model=cbind(1,design_4$design)
model=as.matrix(model)
MM=(1/nrow(model)*t(model)%*%model)
D=det(MM)^(1/ncol(MM))
#####D-optimal
m=ncol(data_4)
a=1
b=(1+(ncol(data_4)-2)*(1/3))
c=(1-2*(1/3))
D_optimal=(a*b^(m-1)*c^((m-1)*(m-2)/2))^(1/ncol(MM))
####D-efficiency
DE=D/D_optimal
DE
#####D_optimal^(1/ncol(Moment_matrix))
