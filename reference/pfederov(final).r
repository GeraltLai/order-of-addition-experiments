####################################################################################################
### this program is used to generate optimal order-of-addition designs using Federov's algorithm ###
####################################################################################################

rm(list=ls())
ptm=Sys.time()
### load package ###
library(AlgDesign)
library(doParallel)
library(gtools)

### read exterior r script ###
source(file="C:/Users/lab202/Desktop/study/Code-Tsai/pwo.r")

### program parameter ###
m=6
n=60
r=500
q=m*(m-1)/2
p=q+1

### d-efficiency upper bound ###
dub=((1+(m-2)/3)^(m-1)*(1/3)^((m-1)*(m-2)/2))^(1/p)

### candidate set ###
c=permutations(n=m,r=m,v=1:m,repeats.allowed=FALSE)
w=matrix(0,nrow(c),q)
for (i in 1:nrow(c)) w[i,]=pwo(m,c[i,],t(combn(m,2)))

### create a parallel cluster ###
ncl=4
cl=makeCluster(ncl,type="PSOCK")
registerDoParallel(cl)

for (i in 1:10^4)
{
  
  ### openMP ###
  out=foreach(ii=icount(ncl),.combine=rbind) %dopar%
    {
      library(AlgDesign)
      set.seed(sample(1:10^6,1))
      fea=optFederov(data=w,nTrials=n,approximate=FALSE,criterion="D",nRepeats=r)
      return(c(fea$D,fea$rows))
    }
  
  ### relative d-efficiency ###
  o=which(out[,1]==max(out[,1]))[1]
  s=out[o,-1]
  x=cbind(1,w[s,])
  mm=t(x)%*%x/n
  de=det(mm)^(1/p)
  de=de/dub
  
  #### save optimal design ###
  fn=paste0("C:/Users/lab202/Desktop/data/federov_",m,"_",n,".txt")
  if (de>0.999999999999) write.table(t(s),file=fn,sep=" ",col.names=FALSE,row.names=FALSE,append=T)
}

### stop a parallel cluster ###
stopCluster(cl)
Sys.time()-ptm #test time