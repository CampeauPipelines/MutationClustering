
## Input -- only modify these lines
mutation_table<-read.table("SMARCC2_mutations_locations.txt", header = FALSE, sep = "\t") # Should be a tab separated file 
length = 3642 ## Transcript length
N = 1000000 ## Number of random permutations to do
mutation_list <- as.vector(mutation_table$V1[mutation_table$V2 %in% c("missense", "nonsense")])

## Counters for QC

dist_vector = c()
delta_prime_list = c()
xi_list = c()
xj_list = c()


## Parameters 

  M = length(mutation_list)
  x1 = 0 ## mutation location 1
  x2 = 0 ## mutation location 2
  prod_dist = 1 ## Product of distance
  delta = 0 ## Observed distance
  delta_prime = 0 ## Permuted distance
  sum_p = 0 ## Number of permutations with delta_prime<=delta
  pvalue = 0 
  m = factorial(M)/(factorial(2)*factorial(M-2)) ## Number of mutation combinations


  for (i in 1:(M-1)) {
    
    xi = mutation_list[i]
    #xi_list = c(xi_list, xi)
    for (j in 2:(M-i+1)) {
      
      xj = mutation_list[j+i-1]
      dist = ((abs(xi-xj) + 1)/(length + 1)) + 1
      prod_dist = (prod_dist*dist)
      #dist_vector = c(dist_vector, prod_dist)
      #xj_list= c(xj_list, xj)
      
    } 
    
  } 
  
  delta = prod_dist^(1/m)  
    
  
  for (o in 1:N) {
    
    prod_dist = 1
    
    for (i in 1:(M-1)) {
    
      xi = sample(1:length, 1)
      #xi_list = c(xi_list, xi)
      for (j in 2:(M-i+1)) {
      
        xj = sample (1:length, 1)
        dist = ((abs(xi-xj) + 1)/(length + 1)) + 1
        prod_dist = (prod_dist*dist)
        #dist_vector = c(dist_vector, prod_dist)
        #xj_list= c(xj_list, xj)
        
      } 
    } 
    
    delta_prime = prod_dist^(1/m)  
    # delta_prime_list = c(delta_prime_list, delta_prime)
    
    if (delta_prime <= delta) {
     sum_p = sum_p +1 
     
    }
    
  }

p_value = ((sum_p + 1)/(N + 1))

p_value
