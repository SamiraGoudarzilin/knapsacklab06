

#' brutforce knapsack
#'
#' @param x vector
#' @param W vector
#' @param parallel logical 
#'
#' @return list
#' @export
brute_force_knapsack <- function(x, W, parallel=FALSE){
  #if(typeof(x) != "list"){stop("x must be a dataframe with column names w and v")}
  if(W < 0){stop("W must not be negative")}
  n=nrow(x)
  max_value<-0
  best_elements <- 0
  env_check <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  
  if (nzchar(env_check) && env_check == "TRUE") {
    cores <- 2L
  } else {
    cores <- parallel::detectCores()
  }
  
  cl <- parallel::makeCluster(cores)
  if(parallel){
    values <- x$v
    weights <- x$w
    result <- parallel::parLapply(
      cl, 
      1:(2^n-1),
      function(i, W){
        elements <- which(intToBits(i)==1)
        if (sum(weights[elements]) <= W){
          
          return(sum(values[elements]))
          
        }else{
          return(-1)
        }
      },
      W = W
    )
    parallel::stopCluster(cl)
    
    result2 <- unlist(result)
    max_value <- max(result2)
    return(list(
      value = max_value ,
      elements = which(intToBits(which(result2 == max_value))==1)
    ))
    
  }
  
  
  #normal
  for (i in 1:(2^n-1)) {
    
    elements <- which(intToBits(i)==1)
    
    elements <- as.vector(which(intToBits(i)==1))
    w1 <- sum(x[elements, "w"])
    
    
    if (w1 <= W){
      v1 <- sum(x[elements, "v"])
      if(v1>max_value){
        max_value <- v1
        best_elements <- elements
      }
    }
  }
  
  result <- list(value = max_value, elements =best_elements)
  parallel::stopCluster(cl)
  return(result)
}