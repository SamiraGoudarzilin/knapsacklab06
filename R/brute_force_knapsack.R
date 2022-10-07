RNGversion(min(as.character(getRversion()),"3.5.3"))

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
# Implement a function you call knapsack brute force(x, W) that 
#takes a data.frame cx with two
# variables v and w and returns the maximum knapsack value and 
#which elements (rows in the data.frame).
#The variable W is the knapsack size.
# brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
# $value
# [1] 16770
# $elements
# [1] 5 8
brute_force_knapsack <- function(x, W){
  n=nrow(x)
  values<- NULL
  selected_elements <- vector(mode = "list")
  for (i in 1:((2^n)-1)) {
    #print(i)
    w1 <-0
    elements <- as.vector(which(intToBits(i)==1))
    for (j in 1:length(elements)) {
      w=knapsack_objects$w[elements][j]
      w1= w+w1
      #print(w1)
      
    }
    if (w1 < W){
      v1 <- 0
      for (k in 1:length(elements)) {
        vv = knapsack_objects$v[elements][k]
        v1 = vv+v1
      }
      values <- append(values,v1) 
      selected_elements <- append(selected_elements,list(elements))
    }
    
  }
  v <- max(values)
  index <- which(values == v)
  ele <- selected_elements[index]
  result <- list(value = v, elements = unlist(ele))
  return(result)
}
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)