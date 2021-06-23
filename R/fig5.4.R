fig5.4 <- function(mu = 0.05, sigma = 0.05, viewcode = FALSE) {
  #' Density Independent Stochastic Model Demonstration
  #'
  #' Generate and plot (Figure 5.4) 20 versions of the density independent stochastic model, with specified mean log-lambda (mu) and standard deviation (sigma).  Each line is a unique model outcome.
  #' @param mu the mean of the log-lambdas (default to 0.05)
  #' @param sigma the standard deviation of the log-lambdas (default to 0.05)
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #' @export
  #' @examples
  #' # View Figure 5.4
  #' fig5.4()
  #' # View with more inter-annual variation in the log-lambdas
  #' fig5.4(mu = 0.05, sigma = 0.1)
  #' #' # View the code
  #' fig5.4(viewcode)
  #'
  n.years <- 40
  n.start <- 10

  output <- matrix(NA, nrow = n.years, ncol = 20)

  # stochastic model
  for (i in 1:20) {
    rs <- rnorm(n.years, mu, sigma)
    nt <- rep(NA, times = n.years)
    nt[1] <- n.start


    for (t in 2:n.years)
      nt[t] <- nt[t - 1] * exp(rs[t - 1])
    output[, i] <- nt
  }


  # make plot
  reset_graphics_par()
  cols <- viridis::cividis(20)
  ylim <- c(0, 120)

  plot(
    1:n.years,
    output[, 1],
    type = "l",
    lwd = 3,
    col = cols[1],
    xlab = "Years",
    ylab = "Population Size",
    xaxs = "i",
    yaxs = "i",
    ylim = ylim,
    las = 1,
    cex.lab = 1.25
  )

  for (i in 2:20) {
    lines(1:n.years,
          output[, i],
          lwd = 3,
          col = cols[i])
  }
  if(viewcode) cat('  # initialize model run
  n.years <- 40
  n.start <- 10

  output <- matrix(NA, nrow = n.years, ncol = 20)

  # Run stochastic model
  for (i in 1:20) {
    rs <- rnorm(n.years, mu, sigma)
    nt <- rep(NA, times = n.years)
    nt[1] <- n.start
    for (t in 2:n.years)
      nt[t] <- nt[t - 1] * exp(rs[t - 1])
    output[, i] <- nt
  }


  # make plot
  # get 20 colors from cividis color palette
  cols <- viridis::cividis(20)


  plot(
    1:n.years,
    output[, 1],
    type = "l",
    lwd = 3,
    col = cols[1],
    xlab = "Years",
    ylab = "Population Size",
    xaxs = "i",
    yaxs = "i",
    ylim = c(0,120),
    las = 1,
    cex.lab = 1.25
  )

  for (i in 2:20) {
    lines(1:n.years,
          output[, i],
          lwd = 3,
          col = cols[i])
  }', sep = "\n")
}
