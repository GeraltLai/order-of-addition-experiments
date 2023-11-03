####################################################################################################
############### this program is used to search for minimal designs using union method ##############
####################################################################################################

rm(list=ls())

### program parameters ###
m=5
s=1

### read order-of-addition design ###
q=as.matrix(read.table(file=paste0("quasi_",m,".txt"),sep="",header=F))

### plus set and minus set ### 
qp=q[,1:(ncol(q)/2)]
qm=q[,(ncol(q)/2+1):ncol(q)]
qi=seq(1,(ncol(q)/2),2)

### search minimal design ###
tic=Sys.time()
min.n=length(unique(as.vector(q)))
min.p=1:length(unique(as.vector(q)))
for (i in 1:10^6)
{
  ### union method ###
  p=NULL
  for (j in 1:nrow(q))
  {
    kp=sample(qi,s)
    km=sample(qi,s)
    for (k in 1:s) p=c(p,qp[j,kp[k]:(kp[k]+1)],qm[j,km[k]:(km[k]+1)])
  }
  p=unique(p)
  n=length(p)
  
  ### update minimal design ###
  if (n<min.n)
  {
    min.n=n
    min.p=sort(p)
  }
}
toc=Sys.time()

### output ###
toc-tic
m
s
min.n
