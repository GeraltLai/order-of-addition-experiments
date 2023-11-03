#####
rm(list=ls())
library(AlgDesign)##optFedreov
library(gtools)##permutations packages
#####
m=7 ##components
data=permutations(m,m,v=1:m,repeats=F)

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

#####run PWO-Function
model=PWO(data)
model=cbind(1,model)
MM=(1/nrow(data))*(t(model)%*%model)
D=det(MM)^(1/ncol(MM))

#####D-optimal
a=1
b=(1+(m-2)*(1/3))
c=(1-2*(1/3))
D_optimal=(a*b^(m-1)*c^((m-1)*(m-2)/2))^(1/ncol(MM))

#####D-efficiency
D/D_optimal

