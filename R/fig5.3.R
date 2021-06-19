fig5.3 <- function() {
  #' Effect of Stochasticity on Average Population Size
  #'
  #' Demonstration of how variability in population growth rate leads to reduced average population size (Figure 5.3). In each simulation, annual population growth rate, lambda, varies randomly within a specified range.  This is repeated many times, and the average over each simulation is plotted.
  #'
  #' @export
  #' @examples
  #' # View the code
  #' print(fig5.3)
  #' # View Figure 5.3
  #' fig5.3()

run_model <- function(lambda_bar, range) {
  Nt <- rep(NA,100)
  Nt[1] <- N_0
  lambda_t <- runif(100, min = lambda_bar - range, max = lambda_bar + range)

  lambda_t <- lambda_t - mean(lambda_t)+1

  for (i in 2:100) Nt[i] <- Nt[i-1]*lambda_t[i-1]
  return(Nt)
}
output <- matrix(NA, nrow = 100000, ncol = 100)
lambda_bar <-1
N_0 <- 100
cols <- c("black", "gray80", "gray30")
  plot(1:100, rep(N_0,100),
       type = "l",
       lwd = 2,
       col = cols[1],
       xlim = c(1,100),
       xlab = "Years",
       ylim = c(80, 110),
       ylab = "Population Size",
       las =1)

  # repeat for range = 0.05
  range <- 0.05
  for (i in 1:100000) output[i,] <- run_model(lambda_bar, range)
  Nt_avg <- colMeans(output)

  lines(1:100, Nt_avg,
        lwd = 2,
        col = cols[2]
  )

  # repeat for range = 0.1
  range <- 0.1
  for (i in 1:100000) output[i,] <- run_model(lambda_bar, range)
  Nt_avg <- colMeans(output)


  lines(1:100, Nt_avg,
        lwd = 2,
        col = cols[3]
  )

  # Add text to plot
  text.labels <- c(expression(paste(lambda["t"], "=1")),
                   expression(paste(0.95, "<",lambda["t"], "<",1.05)),
                   expression(paste(0.9, "<",lambda["t"], "<",1.10))
  )

  text(x = rep(100,3), y  = c(101, 95, 84),
       labels = text.labels,
       pos = 2,
       cex = 1.25)

}
