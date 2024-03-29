calcpi <- function(n.iters = 100000, viewcode = F) {
  #' Monte carlo simulation to estimate pi.
  #'
  #'
  #' Demonstration of example in 14.3.1 "What is pi".  This uses Monte Carlo simulation uses the ratio of randomly drawn points that are in a circle of radius r to a square with length r.
  #'
  #' @param n.iters the number of Monte Carlo iterations.  Must exceed 1000.
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #'
  #' @return Monte Carlo based estimate of pi
  #' @export
  #'
  #' @examples
  #' # use a small number of iterations, likely get a poor estimate of pi
  #' pi.est <- calcpi(n.iters = 1000)
  #' # get a more robust estimate, but very slow
  #' pi.est <- calcpi(n.iters = 10000000)
  #' # run and view function code
  #' pi.est <- calc.pi(printfun = TRUE)
  #'
  #'
  #'

  if(n.iters<1000) stop("n.iters should be greater than or equal to 1000")
in.circle <- function(x,y)  ifelse(sqrt(x^2+y^2)<= 1, 1, 0)


in.square <- function(x,y) ifelse(abs(x) <= 0.5 & abs(y) <= 0.5, 1, 0)

output <- matrix(NA, nrow = n.iters, ncol = 2)
for (i in 1:n.iters) {
  x <- runif(n = 1, min = -1, max =1)
  y <- runif(n = 1, min = -1, max =1)
  output[i,1] <-in.circle(x,y)
  output[i,2] <- in.square(x,y)
}

if(viewcode) cat("# Create function to calculate whether a random draw is within the circle boundaries
  in.circle <- function(x,y)  ifelse(sqrt(x^2+y^2)<= 1, 1, 0)

  # Create function to calculate whether a random draw is within the square boundaries
  in.square <- function(x,y) ifelse(abs(x) <= 0.5 & abs(y) <= 0.5, 1, 0)

  # set up output to store results
  output <- matrix(NA, nrow = n.iters, ncol = 2)
  # run Monte Carlo Simulation
  for (i in 1:n.iters) {
    x <- runif(n = 1, min =   -1, max =1)
    y <- runif(n = 1, min = -1, max =1)
    output[i,1] <-in.circle(x,y)
    output[i,2] <- in.square(x,y)
  }", sep = "\n")
return(sum(output[,1]) / sum(output[,2]))
}
