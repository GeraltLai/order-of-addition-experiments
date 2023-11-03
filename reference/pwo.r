pwo=function(m,p,idx)
{
  r=rep(-1,length(p),m*(m-1)/2)
  for (j in 1:length(idx[,1])) 
  {
    for (k in 1:length(idx[,1]))
    {  
      if (p[idx[j,1]]==idx[k,1] && p[idx[j,2]]==idx[k,2]) r[k]=1
    }
  }
  return(r)
}