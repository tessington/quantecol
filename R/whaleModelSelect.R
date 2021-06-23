whaleModelSelect <- function(viewcode = FALSE) {
  #' Model Selection: Population Dynamics of Killer Whales
  #'
  #' This function generates Figure 9.3 and table 9.1 by fitting a density independent, a logistic, and a beverton-holt model to the southern resident killer whale population counts.  This illustrates the estimation of population parameters assuming "process" error, and the calculation of AIC and delta AIC
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code  #'
  #' @return a matrix containing negative log likelihood, AIC, and delta AIC for each model
  #' @export
  #' @examples
  #' # generate plot in Fig 9.3 and get table 9.1 returned
  #' AIC.table <- whale_model_select()
  #' print(AIC.table)
  #' #view  commands
  #' whale_model_select(viewcode = TRUE)


thedata <- killerwhales
ndata <- nrow(thedata)

Nt <- thedata[-ndata,2]
Ntplus1 <- thedata[-1,2]
reset_graphics_par()
plot(Nt, Ntplus1,
     type ="p",
     pch = 21,
     bg = "black",
     xlab = expression("N"["t"]),
     ylab = expression("N"["t+1"]),
     xlim =c(0,105),
     ylim = c(0,105),
     las =1,
     yaxs = "i",
     xaxs = "i")


fitdi <- function(pars, thedata){
  lambda <- pars[1]
  ndata <- nrow(thedata)
  n.t <- thedata[-ndata,2]
  ntplus.one.hat <- n.t * lambda
  ntplus.one.obs <- thedata[-1,2]

  nll <- -sum(dpois(ntplus.one.obs, ntplus.one.hat, log = T))

  return(nll)
}


fit.logistic <- function(pars, thedata){
  r <- exp(pars[1])
  k <- exp(pars[2])
  ndata <- nrow(thedata)
  n.t <- thedata[-ndata,2]
  ntplus.one.obs<- thedata[-1, 2]
  ntplus.one.hat <- n.t + n.t* r * (1 - n.t / k)
  nll <- - sum(dpois(ntplus.one.obs, ntplus.one.hat, log = T))
  return(nll)
}

fit.bh <- function(pars, thedata) {
  alpha <- exp(pars[1])
  beta <- exp(pars[2])
  ndata <- nrow(thedata)
  n.t <- thedata[-ndata,1]
  n.t <- thedata[-ndata,2]
  ntplus.one.obs<- thedata[-1, 2]
  ntplus.one.hat <- n.t + n.t* alpha / ( 1 + n.t * beta)
  nll <- - sum(dpois(ntplus.one.obs, ntplus.one.hat, log = T))
  return(nll)


}

startpars <- 1.002
solve.di <- optim(par = startpars,
    fn = fitdi,
    thedata = thedata,
    method = "Brent",
    lower = 0.5,
    upper = 1.1
  )

startpars <- c(log(0.002), 4)
solve.logistic <-  optim(par = startpars,
    fn = fit.logistic,
    thedata = thedata,
    method = "BFGS"
  )


# fit a BH model
startpars <- c(log(0.002), log(.01))
solve.bh <-optim(par = startpars,
    fn = fit.bh,
    thedata = thedata,
    method = "BFGS"
  )

AICtable <- matrix(NA, nrow = 3, ncol = 3)
rownames(AICtable) <- c("Density independent", "Logistic", "Beverton-Holt")
colnames(AICtable) <- c("NLL", "AIC", "Delta AIC")
AICtable[,1] <- c(solve.di$value, solve.logistic$value, solve.bh$value)
AICtable[,2] <- 2 * AICtable[,1] + 2 * c(1,2,2)
AICtable[,3] <- AICtable[,2] - min(AICtable[,2])

if(viewcode) cat('thedata <- killerwhales
ndata <- nrow(thedata)

Nt <- thedata[-ndata,2]
Ntplus1 <- thedata[-1,2]

plot(Nt, Ntplus1,
     type ="p",
     pch = 21,
     bg = "black",
     xlab = expression("N"["t"]),
     ylab = expression("N"["t+1"]),
     xlim =c(0,105),
     ylim = c(0,105),
     las =1,
     yaxs = "i",
     xaxs = "i")


fitdi <- function(pars, thedata){
  lambda <- pars[1]
  ndata <- nrow(thedata)
  n.t <- thedata[-ndata,2]
  ntplus.one.hat <- n.t * lambda
  ntplus.one.obs <- thedata[-1,2]

  nll <- -sum(dpois(ntplus.one.obs, ntplus.one.hat, log = T))

  return(nll)
}


fit.logistic <- function(pars, thedata){
  r <- exp(pars[1])
  k <- exp(pars[2])
  ndata <- nrow(thedata)
  n.t <- thedata[-ndata,2]
  ntplus.one.obs<- thedata[-1, 2]
  ntplus.one.hat <- n.t + n.t* r * (1 - n.t / k)
  nll <- - sum(dpois(ntplus.one.obs, ntplus.one.hat, log = T))
  return(nll)
}

fit.bh <- function(pars, thedata) {
  alpha <- exp(pars[1])
  beta <- exp(pars[2])
  ndata <- nrow(thedata)
  n.t <- thedata[-ndata,1]
  n.t <- thedata[-ndata,2]
  ntplus.one.obs<- thedata[-1, 2]
  ntplus.one.hat <- n.t + n.t* alpha / ( 1 + n.t * beta)
  nll <- - sum(dpois(ntplus.one.obs, ntplus.one.hat, log = T))
  return(nll)


}

startpars <- 1.002
# solve density independent model
solve.di <- optim(par = startpars,
    fn = fitdi,
    thedata = thedata,
    method = "Brent",
    lower = 0.5,
    upper = 1.1
  )
# solve logistic model
startpars <- c(log(0.002), 4)
solve.logistic <-  optim(par = startpars,
    fn = fit.logistic,
    thedata = thedata,
    method = "BFGS"
  )


# solve beverton holt model
startpars <- c(log(0.002), log(.01))
solve.bh <-optim(par = startpars,
    fn = fit.bh,
    thedata = thedata,
    method = "BFGS"
  )
# create AIC table
AICtable <- matrix(NA, nrow = 3, ncol = 3)
rownames(AICtable) <- c("Density independent", "Logistic", "Beverton-Holt")
colnames(AICtable) <- c("NLL", "AIC", "Delta AIC")
AICtable[,1] <- c(solve.di$value, solve.logistic$value, solve.bh$value)
AICtable[,2] <- 2 * AICtable[,1] + 2 * c(1,2,2)
AICtable[,3] <- AICtable[,2] - min(AICtable[,2])
', sep = "\n")
return(AICtable)

}
