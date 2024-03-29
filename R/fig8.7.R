fig8.7 <- function(viewcode = FALSE) {
  #' Maximum Likelihood Estimate of Catchability
  #'
  #' Generate Figure 8.7 that shows negative log-likelihood for different values of Hector dolphin catchability (q), and return the maximum likelihood estimate and associated confidence interval
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #' @return a list object containing maximum likelihood estimat and confidence interval
  #' @export
  #' @examples
  #' # Recreate fig 8.8 and assign output to object 'fit'
  #' fit <- fig8.7()
  #' print(fit)
  #' # View the code
  #' fig8.7(viewcode = TRUE)



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
if(viewcode) cat('k <- c(79, 70)
E <- c(0.267, 0.213)
N <- c(1024, 990)

# set up list of qs and empty array to store the results
                 qlist <- seq(from = 0.2, to = 0.45, by = 0.001)
                 nll.list <- rep(x = NA, times = length(qlist))

                 # cycle through qlist, for each, calculate the binomial probabilities and then calculate likelihood
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

                 ', sep = "\n")
return(list(q.mle = q.mle, ci = c(lb, ub)))
}
