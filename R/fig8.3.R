fig8.3 <- function() {

  #' Poison Maximum Likelihood Estimation, single observation
  #'
  #' This function generates  Figure 8.3 and returns the maximum likelihood estimate of r and the confidence interval
  #'
  #' @return a list object containing the maximum likelihood estimate of r and the confidence interval
  #' @export
  #'
  #' @examples
  #' #view  commands
  #' print(fig8.3)
  #' # generate plot shown in Fig 8.3
  #' fig8.3()
  #' $mle
  #' [1] 0.1
  #' $ci
  #' [1] 0.0175 0.3075
  #'
  #' # generate a plot with an unstable model
  #' fig4.1("unstable")
  #'
  thedata <- 2
  t <- 20
  nll.fun <-
    function(r, thedata, t)
      - sum(dpois(
        x = thedata,
        lambda = r * t,
        log = TRUE
      ))

  r.list <- seq(from = 0.01, to = 0.5, by = 0.0025)
  nll.list <- rep(x = NA, times = length(r.list))

  for (i in 1:length(nll.list)) {
    nll.list[i] <- nll.fun(r.list[i], thedata, t)
  }

  min.nll <- min(nll.list)
  r.mle <- r.list[nll.list == min.nll]
  target.nll <- min.nll + 1.92
  ci.index <- which(nll.list <= target.nll)
  lb <- r.list[min(ci.index)]
  ub <- r.list[max(ci.index)]

  reset_graphics_par()

  plot(
    r.list,
    nll.list,
    type = "l",
    lwd = 3,
    xlim = c(0, 0.5),
    ylim = c(0, 8),
    ylab = "Negative log-likelihood",
    xlab = "Geoduck density (r)",
    xaxs = "i",
    yaxs = "i"
  )
  lines(c(lb, ub), rep(target.nll, 2),
        col = "gray",
        lwd = 3)
  abline(h = min.nll, lty = "dashed", lwd = 3)
  return(list(mle = r.mle, ci = c(lb, ub)))
}
