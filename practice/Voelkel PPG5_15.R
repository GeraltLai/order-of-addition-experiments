####
rm(list=ls())
library(AlgDesign)##optFedreov
library(gtools)##permutations packages


#####load-data
data=read.csv(file="C:/Users/stupi/Desktop/study/order of addition/Voelkel_2019_QE-data/PPG5_15.csv",header=T)
order=data[,1:5]

#####change order-of-addition
design=matrix(data=0,nrow=15,ncol=5)
for(i in 1:15)
  for(j in 1:5)
    for(k in 1:5){
      if(order[i,j]==k){
        design[i,k]=j
      }
    }

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

#####interaction function2
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

#####run function
model1=PWO(design)
model2=inter(design)
model=cbind(model1,model2)

#####linear model
y=data$log.L.H
B12=data$X.R1
B13=data$X.R2
B14=data$X.A
B15=data$X.M1
B23=data$R1.R2
B24=data$R1.A
B25=data$R1.M1
B34=data$R2.A
B35=data$R2.M1
B45=data$A.M1
B123_1=model[,11]
B123_2=model[,12]

full=lm(y~B12+B13+B14+B15+B23+B24+B25+B34+B35+B45+B123_1+B123_2)
summary(full)
anova(full)


#####H0 B123_1=B123_2=0
redu_1=lm(y~B12+B13+B14+B15+B23+B24+B25+B34+B35+B45)
anova(redu_1,full)



