RNGversion(min(as.character(getRversion()),"3.5.3"))

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

#'Greedy Function Knapsack
#'
#' @param x is a dataframe containing 2 elements w and v.
#' @param W is the maximum weight that should be in a pack.
#'
#' @return Maximum value of all objects in the pack. 
#' @export
#'
#' @examples
greedy_knapsack <- function(x,W ) {
        if (typeof(x) != "list") {stop(" X should be a list")}
        if(W < 0){stop("W must not be negative")}
  
        n <- nrow(x)
        x <- cbind(x,elements = c(1:n))
        x$vpw <- ((x$v)/(x$w)) # calculate value per weight for each object
        x <- x[order(x$vpw,decreasing = TRUE), ] # sort by value per weight
        elements_knapsack <- c()
        packed_wight <- 0
        packed_value <- 0
        i <- 1
        while (packed_wight < W) {
          packed_wight <- x$w[i] + packed_wight
          i <- i+1
        }
          elements_knapsack <- append(elements_knapsack,x$elements[1:(i-2)])
          packed_value <- sum(x$v[1:(i-2)] )
          result <- list(value = round(packed_value),elements = elements_knapsack)
          return(result)
}        


greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
