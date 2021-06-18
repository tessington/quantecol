fig8.7 <- function() {
  #' Maximum Likelihood Estimate of Catchability
  #'
  #' Generate Figure 8.78 that shows negative log-likelihood for different values of Hector dolphin catchability (q), and return the maximum likelihood estimate and associated confidence interval
  #' @return a list object containing maximum likelihood estimat and confidence interval
  #' @export
  #' @examples
  #' # View the code
  #' print(fig8.7)
  #' # Recreate fig 8.8 and assign output to object 'fit'
  #' fit <- fig8.7()
  #' print(fit)


k <- c(79, 70)
E <- c(0.267, 0.213)
N <- c(1024, 990)


qlist <- seq(from = 0.2, to = 0.45, by = 0.001)

nll.list <- rep(x = NA, times = length(qlist))

for (i in 1:length(qlist)) {
  p <- E * qlist[i]
  nll.list[i] <- -sum(dbinom(k, size = N, prob = p, log = T))
}

min.mle <- min(nll.list)
q.mle <- qlist[nll.list== min.mle]
target.nll <- min.mle + 1.92
ci.range <- which(nll.list <= target.nll)
lb <- qlist[min(ci.range)]
ub <- qlist[max(ci.range)]

# Make plot
reset_graphics_par()
plot(qlist, nll.list,
     type = "l",
     lwd = 3,
     ylim = c(6, 20),
     xlim = c(0.2, 0.45),
     xlab = "Catchability (q)",
     ylab = "Negative log-likelihood",
     las = 1
)
lines(c(lb, ub), rep(target.nll, 2), lwd = 3, col = "gray")

return(list(q.mle = q.mle, ci = c(lb, ub)))
}
