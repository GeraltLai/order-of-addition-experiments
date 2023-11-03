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
full_model=subset(model,model[,1]==1) ##pick up z12=1
MM=(1/nrow(full_model)*t(full_model)%*%full_model)
D=det(MM)^(1/ncol(MM))
eigen=eigen(MM)
#####optFederov
model=as.data.frame(model)
new_model=subset(model,model[,1]==1)
design=new_model[,-1]
opt_design=optFederov(~.,design,criterion = "D",nTrials=24,nRepeats=5000)
row=design[opt_design$rows,]
x=as.integer(row.names(row))
new_data=data[x,]
design=as.matrix(opt_design$design)
design=cbind(1,design)
MMD=(1/nrow(design)*t(design)%*%design)
FD=det(MMD)^(1/ncol(MMD))

#####D-optimal
m=ncol(data)
b=(1+(ncol(data)-2)*(1/3))
c=(1-2*(1/3))
D_optimal=((b^(m-1))*(c^((m-1)*(m-2)/2)))^(1/ncol(MM))
####D-efficiency
DE=FD/D_optimal
DE