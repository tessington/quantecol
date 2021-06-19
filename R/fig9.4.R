fig9.4 <- function(pmf = "poisson", viewcode = FALSE) {
  #' Fit Song Sparrow Reproductive Success
  #'
  #' This function generates Figure 9.4 by estimating the maximum likelihood parameter values of the ricker function, and plotting the best fit and generating an AIC table showing comparison between the Ricker function and a density independent function.  The default probability mass function is Poisson.
  #'
  #' @param pmf a character string that is either "poisson" or "pmf", indicating the probabiilty function to be used in model fitting
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #' @return A matrix showing negative log likelihood, AIC, and delta AIC
  #' @export
  #'
  #' @examples
  #' # generate plot in Fig 9.4 and get AIC table
  #' fig9.4()
  #' # Repeat using negative binomial model
  #' fig9.4(pmf = "nbinom")
  #' #' #view  commands
  #' fig9.4(viewcode = TRUE)


pmf <- tolower(pmf)
if(!pmf %in% c("poisson", "nbinom")) stop("function input 'pmf' must be either 'poisson' or 'nbinom'")

thedata <- songsparrows
n.female <- songsparrows$Females
n.offspring <- songsparrows$IndependentOffspring

fit.di <- function(par, n.female, n.offspring) {
  alpha <- par[1]
  expected.offspring <- n.female *  alpha
  nll <- -sum(dpois(x = n.offspring,
                    lambda = expected.offspring,
                    log = T))
  return(nll)
}

fit.ricker <- function(par, n.female, n.offspring) {
  alpha <- par[1]
  beta <- par[2]
  expected.offspring <- n.female *  alpha * exp(-beta * n.female)
  nll <- -sum(dpois(x = n.offspring,
                    lambda = expected.offspring,
                    log = T))
  return(nll)
}

fit.di.nb <- function(par, n.female, n.offspring) {
  alpha <- par[1]
  k <- par[2]
  expected.offspring <- n.female *  alpha
  nll <-  -sum(dnbinom(x = n.offspring,
                       mu = expected.offspring,
                       size = k,
                       log = T))
  return(nll)
}


fit.ricker.nb <- function(par, n.female, n.offspring) {
  alpha <- par[1]
  beta <- par[2]
  k <- par[3]
  expected.offspring <- n.female *  alpha * exp(-beta * n.female)
  nll <- -sum(dnbinom(x = n.offspring,
                    mu = expected.offspring,
                    size = k,
                    log = T))
  return(nll)
}


if(pmf == "poisson") {
start.par <- 5
solve.di <- optim(par = start.par,
                  fn = fit.di,
                  n.female = n.female,
                  n.offspring = n.offspring,
                  method = "Brent",
                  lower = 1,
                  upper = 20)

start.par <- c(5, 0.01)

solve.ricker <- optim(par = start.par,
             fn = fit.ricker,
             n.female = n.female,
             n.offspring = n.offspring,
             method = "Nelder-Mead")

di.fit <- solve.di
dd.fit <- solve.ricker
npars <- c(1,2)
}

if(pmf == "nbinom") {
  start.par <- c(5, 10)
  solve.di <- optim(par = start.par,
                    fn = fit.di.nb,
                    n.female = n.female,
                    n.offspring = n.offspring,
                    method = "Nelder-Mead"
                    )

  start.par <- c(5, 0.01, 10)

  solve.ricker <- optim(par = start.par,
                        fn = fit.ricker.nb,
                        n.female = n.female,
                        n.offspring = n.offspring,
                        method = "Nelder-Mead")

  di.fit <- solve.di
  dd.fit <- solve.ricker
  npars <- c(2,3)
}

reset_graphics_par()
# Plot the data
plot(n.female, n.offspring,
     type = "p",
     pch = 21,
     bg = "black",
     xlab = "Number of Females",
     ylab = "Number of independent offspring",
     xlim = c(0, 75),
     ylim = c(0, 170),
     xaxs = "i",
     yaxs = "i",
     las = 1)

# plot best fit
n.female.list <- 0:80
alpha.mle <- dd.fit$par[1]
beta.mle <- dd.fit$par[2]

n.hat <- alpha.mle * n.female.list * exp(-beta.mle * n.female.list)
lines(n.female.list, n.hat,
      lwd = 3)
AIC.table <- matrix(NA, nrow = 2, ncol = 3)
colnames(AIC.table) <- c("NLL", "AIC", "delta AIC")
rownames(AIC.table) <- c("Density Independent", "Ricker")
AIC.table[,1] <- c(di.fit$value,dd.fit$value)
AIC.table[,2] <- 2 * AIC.table[,1] + 2 * npars
AIC.table[,3] <- AIC.table[,2] - min(AIC.table[,2])
if(viewcode) cat(readLines(con = "txt/fig9.4.txt"), sep = "\n")
return(AIC.table)
}
