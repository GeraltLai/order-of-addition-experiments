####
rm(list=ls())
library(AlgDesign)##optFedreov
library(gtools)##permutations packages

#####load-data
data=read.csv(file="C:/Users/stupi/Desktop/study/order of addition/Voelkel_2019_QE-data/PPG4_24.csv",header=T)
order=data$Order.of.Addition
m=4
  
#####change character to inter
design=NULL
for(i in 1:nrow(data)){
  A=strsplit(order[i],split="",fixed=T)
  AA=c(A[[1]][1],A[[1]][2],A[[1]][3],A[[1]][4])
  for(i in 1:m){
    if(AA[i]=="A")
      AA[i]=1
    else if(AA[i]=="B")
      AA[i]=2
    else if(AA[i]=="C")
      AA[i]=3
    else if(AA[i]=="D")
      AA[i]=4
  }
  AA=as.integer(AA)
  design=rbind(design,AA)
}
row.names(design)=c(1:nrow(data))

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
B12=data$R1.R2
B13=data$R1.R2
B14=data$R1.A
B23=data$R2.A
B24=data$R2.M1
B34=data$A.M1
B123_1=model[,7]
B123_2=model[,8]
full=lm(y~B12+B13+B14+B23+B24+B34+B123_1+B123_2)
summary(full)
anova(full)


#####H0 B123_1=B123_2=0
redu_1=lm(y~B12+B13+B14+B23+B24+B34)
anova(redu_1,full)



