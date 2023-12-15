#### start
rm(list=ls())
library(AlgDesign)##optFedreov
library(gtools)##permutations packages
source("C:/Users/LAI/Desktop/test/PWO_model.r")
source("C:/Users/LAI/Desktop/test/latin_design.r")

### setting parameter 
m = 4
q = m * (m-1)/2
p = q + 1
r = 6
n = r * m
s = factorial(m - 1)
o = 0 ### initial D-efficiency
run = 0 

### D-efficiency upper bound 
D_optimal = ((1+(m-2)/3)^(m-1)*(1/3)^((m-1)*(m-2)/2))^(1/p)

### candidate set
cs = permutations(n = m-1, r = m-1, v = 1:(m-1), repeats=F)
total_comb = combn(s,r)
total_run = choose(s,r)
final_D = matrix(data = 0, nrow = n, ncol = m)

### exchange algorithm
while(o < 1 & run < total_run){
  run = run + 1
  set = total_comb[,run]
  cr = cs[set, ]
  d_plus = latin_design(m = m, r = r, n = n)
  ### D-efficiency
  model = PWO(d_plus)
  mm = t(model) %*% model/n
  D = det(mm)^(1/p)
  if(D == "NaN") D = 0
  DE = D/D_optimal
  if(o < DE){
    o = DE
    final_D = d_plus
    print(o)
  }
  print(run)
}

### design
print(d_plus)

