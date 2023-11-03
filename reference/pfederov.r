####################################################################################################
### this program is used to generate optimal order-of-addition designs using Federov's algorithm ###
####################################################################################################

rm(list=ls())
options(digits=16)
t0=Sys.time()

### load package ###
library(AlgDesign)
library(doParallel)
library(gtools)

### read exterior r script ###
source(file="pwo.r")

### program parameter ###
m=5
n=60
r=10

### d-efficiency upper bound ###
dub=((1+(m-2)/3)^(m-1)*(1/3)^((m-1)*(m-2)/2))^(1/(1+m*(m-1)/2))

### candidate set ###
pp=permutations(n=m,r=m,v=1:m,repeats.allowed=FALSE)
zz=matrix(0,nrow(pp),m*(m-1)/2)
idx=t(combn(m,2))
for (i in 1:nrow(pp)) zz[i,]=pwo(m,pp[i,],idx)

### openMP ###
ncl=8
cl=makeCluster(ncl,type="PSOCK")
registerDoParallel(cl)
out=foreach(ii=icount(ncl),.combine=rbind) %dopar%
{
  library(AlgDesign)
  set.seed(sample(1:10^6,1))
  fea=optFederov(data=zz,nTrials=n,approximate=FALSE,criterion="D",nRepeats=r)
  return(c(fea$D,fea$rows))
}
stopCluster(cl)

### relative d-efficiency ###
opt=which(out[,1]==max(out[,1]))
if (length(opt)>1) opt=opt[1]
r=out[opt,-1]
p=pp[r,]
z=zz[r,]
x=cbind(1,z)
mm=t(x)%*%x/nrow(x)
de=det(mm)^(1/ncol(x))
de=de/dub
t1=Sys.time()

#### save optimal design ###
sn=sample(10^3:10^4-1,1)
output1=paste0("federov_",m,"_",n,".txt")
output2=paste0("federov_r_",m,"_",n,".txt")
if (de>0.999999999999) 
{
  write.table(p,file=output1,sep=" ",col.names=FALSE,row.names=FALSE)
  write.table(t(r),file=output2,sep=" ",col.names=FALSE,row.names=FALSE)
}
t0
t1
de
r
