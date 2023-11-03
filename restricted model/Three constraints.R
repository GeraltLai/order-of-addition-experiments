####
rm(list=ls())
library(AlgDesign)##optFedreov
library(gtools)##permutations packages
#####
data=permutations(6,6,v=1:6)
model=matrix(0,nrow=nrow(data),ncol=choose(ncol(data),2))
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
for(i in 1:nrow(data)){
  X=PWO(data[i,])
  model[i,]=X
}
#####pick up z12=1
full_model=subset(model,model[,1]==1)
#####pick up z13=1
res_model=subset(full_model,full_model[,2]==1)
#####pick up z14=1
res_model=subset(res_model,res_model[,3]==1)
res_model=res_model[,c(-1,-2)]
#####D-optimal
MM=(1/nrow(res_model)*t(res_model)%*%res_model)
D=det(MM)^(1/ncol(MM))
eigen=eigen(MM)
#####optFederov
opt=res_model[,-1]
design=optFederov(data=opt,criterion = "D",nTrials=48,nRepeats=1000)
model=design$design
model=as.matrix(model)
model=cbind(1,model)
MM=(1/nrow(model)*t(model)%*%model)
DO=det(MM)^(1/ncol(MM))
#####D-efficiency
DO/D
