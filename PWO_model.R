### PWO model
PWO = function(S){
  model=matrix(0, nrow = nrow(S), ncol = choose(ncol(S),2))
  for(k in 1:nrow(S)){
    m = ncol(S)
    A = NULL
    for(x in 1:m) 
      for (i in 1:m) {
        if (S[k,i] == x)  
          A=c(A,i)
      }
    A1 = NULL
    for(n in length(A))
      for(i in 1:(n - 1))
        for(j in (i + 1):n)
          if(A[i] >= A[j]){
            A1 = c(A1, -1)
          }else{
            A1 = c(A1, 1)
          }
    X = A1
    model[k,] = X
  }
  model = cbind(1,model)
  return(model)
}
