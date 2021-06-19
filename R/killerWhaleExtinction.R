killerWhaleExtinction <-function(model.type="base", viewcode = FALSE) {
  #' Extinction risk of Southern Resident Killer Whales
  #'
  #' Perform simulations to evaluate the risk that southern resident killer whales will drop below the quasi-extinction threshold (4) over the next 100 years, given the data on total killer whale counts.  The model simulates 10,000 random population runs, under three scenarios
  #' \itemize{
  #' \item "base", where parameters are assumed to be known and the lambdas are independent
  #' \item "autocorrelation" where the log-lambdas are presumed to be autocorrelated
  #' \item "uncertainty" where we admit uncertainty in the estimates of the mean and standard deviation of the log lambdas
  #' }
  #' @param  model.type is character that is either 'base', 'autocorrelation', or 'uncertainty', associated with the three scenarios in Table 5.1.
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #' @return the estimated probability of extinction over the next 100 years, and plots Figures 5.5 and 5.6.
  #' @export
  #' @examples
  #' # Run the base model
  #' extinct.base <- killerWhaleExtinction(model.type = "base")
  #' # Run the uncertainty model
  #' extinct.uncertainty <- killerWhaleExtinction(model.type = "uncertainty")
  #'  # View the code
  #' killerWhaleExtinction(viewcode = TRUE)


  # Error checking
  if(!model.type  %in% c("base", "autocorrelation", "uncertainty")) {
    stop("Incorrect model.type specification.  Must be either 'base', 'autocorrelation', or 'uncertainty'")
  }
thedata <- killerwhales
year.list <- thedata[,1]
alive <- thedata[,2]


plot(year.list, alive,
     type = "l",
     lwd = 2,
     xlab = "Year",
     ylab = "Population Size",
     las = 1)

points(year.list, alive,
       pch = 21,
       bg = "black")


# get r_t
rt <- log(alive[-1]/alive[-length(alive)])
mu <- mean(rt)
sigma <- sd(rt)
se <- sigma / (length(rt))^(.5)

## AUTOCORRELATION   plot

plot(year.list[-length(year.list)], rt,
     type = "l",
     lwd = 2,
     xlab = "Year",
     ylab = expression(paste("r"["t"])),
     las = 1)

points(year.list[-length(year.list)], rt,
       pch = 21,
       bg = "black")

# simulate model for a brief policy relevant time, 100 years
tmax.use <-  100
rho.est <- 0.23
rhat.use <- mu
no.use <- alive[length(alive)]
nextinct.use <- 40
sigma.use <- sigma
df <- length(rt) -1


run.stochastic <- function(pars) {
  with(pars, {
    extinct.flag <- 0
    vt <- rnorm(tmax, mean = 0, sd = sigma)
    nt <- no
    eta<- rt <- nt <- rep(NA, tmax)

    eta[1] <- vt[1]
    nt[1] <- no

    for (t in 2:tmax) eta[t] <- rho * eta[t-1] + vt[t] * sqrt(1- rho^2)
    rt <- rbar + eta
    for (t in 2:tmax)  {
      nt[t] <- nt[t-1] * exp(rt[t-1])
      if (nt[t] <= nextinct) break
    }
    if (nt[t] <= nextinct) extinct.flag <- 1

    return(extinct.flag)
  })
}

n.iters <- 10000
output <- rep(NA, n.iters)

# code to use if model.type is NOT "uncertainty"
if(!model.type == "uncertainty"){
if(model.type == "base") pars <- list(tmax = tmax.use, rho = 0, rbar = mu, sigma = sigma, no = no.use, nextinct = nextinct.use)
if(model.type == "autocorrelation") pars <- list(tmax = tmax.use, rho = rho.est, rbar = mu, sigma = sigma, no = no.use, nextinct = nextinct.use)
for (i in 1:n.iters)  output[i] <- run.stochastic(pars)
extinction <- sum(output) / n.iters
}


# code to use if model.type is "uncertainty"
if(model.type == "uncertainty") {
output <- rep(NA, n.iters)
rhat.2.use <- rnorm(n.iters, mu, se)
sigma.2.use <- sqrt(sigma^2*rchisq(n.iters, df = df)/df)
pars <- list(tmax = tmax.use, rho = 0, no= no.use, nextinct = nextinct.use)
for (i in 1:n.iters) {
  pars$rbar <- rhat.2.use[i]
  pars$sigma <- sigma.2.use[i]
  output[i]<-run.stochastic(pars)
}
extinction = sum(output) / n.iters
}
if(viewcode) cat(readLines(con = "txt/killerWhaleExtinction.txt"), sep = "\n")
return(extinction)


}
