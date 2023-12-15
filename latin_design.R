### latin_design
latin_design = function(m, r, n){
  d_plus = matrix(data = 0, nrow = n, ncol = m)
  for(ii in 1:r){
    latin = matrix(data = 0, ncol = m, nrow = m)
    latin[1,] = c(cr[ii,],m)
    for(i in 2:m){
      cr_latin = latin[i-1,] + 1
      cr_latin = replace(cr_latin, cr_latin > m, 1)
      latin[i,] = cr_latin
    }
    start = m * (ii - 1) + 1
    end = m * ii
    d_plus[start:end, ] = latin
  }
  return(d_plus)
}