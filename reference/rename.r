####################################################################################################
####################### this program is used to rename the outputs of latinoa ######################
####################################################################################################

rm(list=ls())

input=list.files()

for (i in 4:8)
{
  ### read design ###
  c=as.matrix(read.table(file=input[i],sep="",header=FALSE))

  ### write design ###
  output=paste("optimal_6_4_c_",i,".txt",sep="")
  write.table(c,file=output,sep=" ",col.names=FALSE,row.names=FALSE)
}