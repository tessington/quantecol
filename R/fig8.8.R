fig8.8 <- function() {
  #' Fit Survival Function to Goby Data
  #'
  #' This function generates Figure 8.8 by estimating the maximum likelihood parameter values of the survival function in Eq. 8.10
  #'
  #' @return a list object containing the maximum likelihood estimate of pmax and alpha
  #' @export
  #'
  #' @examples
  #' #view  commands
  #' print(fig8.8)
  #' # generate plot in Fig 8.8 and get maximum likelihood estimated
  #' fit <- fig8.8()
  #' print(fit)
  #' $pmax.mle
  #' [1] 0.5002728
  #' $alpha.mle
  #' [1] 0.01271631

n.init <- gobies$initial_number
n.final <- gobies$final_number
nll.fun <- function(par, n.init, n.final) {
  p.max <- par[1]
  alpha <- par[2]
  ps <- p.max * exp(-alpha * n.init)
  return(-sum(dbinom(x = n.final,
                     size = n.init,
                     prob = ps,
                     log = T)))
}

start.par <- c(0.7, 0.01)
fit <- optim(par = start.par,
             fn = nll.fun,
             n.init = n.init,
             n.final = n.final,
             method = "Nelder-Mead")

# Plot the data
reset_graphics_par()
plot(n.init, n.final,
     type = "p",
     pch = 21,
     bg = "black",
     xlab = expression(paste("N"[0])),
     ylab = expression(paste("N"["survive"])),
     xlim = c(0, 90),
     ylim = c(0, 20),
     xaxs = "i",
     yaxs = "i",
     las = 1)

# plot best fit
n.init.list <- 0:80
pmax.mle <- fit$par[1]
alpha.mle <- fit$par[2]
n.hat <- pmax.mle * n.init.list * exp(-alpha.mle * n.init.list)
lines(n.init.list, n.hat,
      lwd = 3)

return(list(pmax.mle = pmax.mle, alpha.mle = alpha.mle))
}
