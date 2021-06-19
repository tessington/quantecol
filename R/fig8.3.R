fig8.3 <- function(viewcode = F) {

  #' Poison Maximum Likelihood Estimation, single observation
  #'
  #' This function generates  Figure 8.3 and returns the maximum likelihood estimate of r and the confidence interval
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #' @return a list object containing the maximum likelihood estimate of r and the confidence interval
  #' @export
  #' @examples
  #' # generate plot shown in Fig 8.3
  #' fig8.3()
  #' #view  commands
  #' fig8.3(viewcode = F)

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
  if(viewcode) cat(readLines(con = "txt/fig8.3.txt"), sep = "\n")
  return(list(mle = r.mle, ci = c(lb, ub)))

}
