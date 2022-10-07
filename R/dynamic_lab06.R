


#' Title dynamic function for knapsack 
#'
#' @param x a dataframe including v and w
#' @param W maximum weight
#'
#' @return matimum value and elements
#' @export
#'
#' @examples
RNGversion(min(as.character(getRversion()),"3.5.3"))

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

knapsack_dynamic <- function(x,W){
  # Check if input x is correct
  # if(!identical(names(x), c("w", "v")) || ncol(x) != 2 || typeof(x) != "list"){stop("x must be a dataframe with column names w and v")}
  # if(W < 0){stop("W must not be negative")}
 
  n <- nrow(x)
  mat <- matrix(0,nrow =n+1, ncol = W+1)
  rownames(mat) <- c(0:n)
  colnames(mat) <- c(0:W)
  
  for (i in 1:n){
    for (j in 1:W){
      if (x$w[i]>j){
        mat[as.character(i),as.character(j)] <-  mat[as.character(i-1),as.character(j)]
      }
      else{
        mat[as.character(i),as.character(j)] <- 
          max(mat[as.character(i-1),as.character(j)],
              mat[as.character(i-1),as.character(j-x$w[i])]+x$v[i])
      }
    }
  }
  el <- NULL
  max_v = mat[as.character(n),as.character(W)]
  max_value = max_v
  # return(max_v)
  for (i in n:1){
    if(max_v %in% mat[as.character(i),]){
      if(max_v %in% mat[as.character(i-1),]){
        next
      }
    }
    el <- append(el,i)
    max_v <- max_v - x$v[i]
  }
  result = c(value = max_value, elements = list(el))
  return(result)
} 

knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
