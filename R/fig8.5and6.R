fig8.5and6 <- function(viewcode = FALSE) {
  #' Negative Binomial Likelihood Profile
  #'
  #' This function generates Figures 8.5 and Figure 8.6, and returns the maximum likelihood estimate of r and k as calculated through grid-based search and by applying numerical optimization methods, and the confidence interval of r
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #' @return a list object containing the maximum likelihood estimate of r and the confidence interval
  #' @export
  #'
  #' @examples
  #' # generate plots shown in Fig 8.5 and Fig 8.6, and save results
  #' soln <- fig8.5and6()
  #' print(soln)
  #' #view code
  #' fig8.5and6(viewcode = TRUE)


thedata <- c(2,1,7,1,2,5)
t <- c(20, 20, 40, 40, 40,20)

rlist <- seq(from = 0.025, to = 0.30, by = 0.0025)
klist <- seq(from = 0.5, to = 20, by =  0.5)


nll.fun <- function(pars, thedata, t) {
  r <- pars[1]
  k <- pars[2]
  nll <- -sum(dnbinom(thedata, size = k, mu = r * t, log = TRUE))
  return(nll)
}

nll.matrix <- matrix(NA, nrow = length(rlist), ncol = length(klist))
for (i in 1:length(rlist)) {
  r <- rlist[i]
  for (j in 1:length(klist)) {
    k <- klist[j]
    nll.matrix[i,j] <- nll.fun(c(r,k), thedata, t)
  }
}


library(viridis)
colors.2.use<-colorRampPalette(rev(plasma(16)),interpolate="spline")(40)
image(
  y = klist,
  x = rlist,
  z = nll.matrix,
  col = colors.2.use,
  xlab = "r",
  ylab = "k",
  las = 1
)


r.profile <- apply(X = nll.matrix, MAR = 1, FUN = min)

reset_graphics_par()
plot(rlist, r.profile,
     type = "l",
     lwd = 3,
     xlab = "r",
     ylab = "Negative log-likelihood profile",
     ylim = c(12.5, 18.7),
     xlim = c(0, 0.3),
     las =1)

# get confidence interval
min.nll <- min(r.profile)
abline(h = min.nll, lwd = 3, lty = "dashed")

r.mle <- rlist[r.profile==min.nll]
target.nll <- min.nll + 1.92
ci.index <- which(r.profile <= target.nll)
lb <- rlist[min(ci.index)]
ub <- rlist[max(ci.index)]
lines(c(lb, ub), rep(target.nll, 2), lwd =4, col = "gray")

k.profile <- apply(X = nll.matrix, MAR = 2, FUN = min)
k.mle <- klist[k.profile==min.nll]

options(warn = -1)
# find maximum likelihood using numerical optimization
start.pars <- c(0.1, 10)
nb.soln <- optim(par = start.pars,
                 fn = nll.fun,
                 thedata = thedata,
                 t = t,
                 method = "Nelder-Mead")

if(viewcode) cat(readLines(con = "txt/fig8.5and6.txt"), sep = "\n")
options(warn = 0)
return(list(r.mle.grid = r.mle, r.mle.numeric = nb.soln$par[1], k.mle.grid = k.mle,
            k.mle.numeric = nb.soln$par[2], r.ci = c(lb, ub)))

}
