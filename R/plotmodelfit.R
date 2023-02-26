plotmodelfit <- function(x,
                       r,
                       N0 = NULL,
                       N1 = NULL,
                       K = NULL,
                       type = "Process",
                       add.to.plot = F
                       ) {

  #' Plot data and fitted model
  #'
  #' @description Makes plot of data and one fitted dynamic model.  Can also add an additional fitted model to an existing plot
  #'
  #' @param x: the data frame containing year and harbor seal abundances
  #' @param r: the fitted parameter r
  #' @param N0: the fitted parameter N0 (only used for process error models)
  #' @param N1: the fitted parameter N1 (only used for observation error models)
  #' @param K: the fitted parameer K (only used for density dependent models
  #' @param type: must be either "Process" or "Observation". Process error plotted as blue line, observation error plotted as red line
  #' @param add.to.plot: if TRUE, then adds fitted model to an existing plot.
  #' @examples
  #'
  #' #Plot best fitting process, density independent model with data
  #'
  #' di.mle.pars <- proc.di.fit$par
  #' plotmodelfun(x = harborseals,
  #' r = di.mle.pars[1],
  #' N0= = di.mle.pars[2])
  #' type = "Process")
  #'
  #' # Add density dependent model fit to existing plot:
  #'
  #' dd.mle.pars <- proc.dd.fit$par
  #' plotmodelfun(x = harborseals,
  #' r = dd.mle.pars[1],
  #' K = dd.mle.pars[2],
  #' N1 = dd.mle.pars[3]
  #' type = "Process",
  #' add.to.plot = TRUE)


  ####### Error Checking######################
  #############################################
  type <- tolower(type)
  if(type == "process" & is.null(N0)) stop("oops! you must provide N0 to plot the Process error model")

  if(type == "observation" & is.null(N1)) stop("oops! you must provide N1 to plot the Observation error model")

  if(!type == "process" & !type == "observation") stop("oops! model type must be either 'Observation' or 'Process'")
  ########### done error checking##############
  #############################################

  nt.obs <- x$abundance
  year <- x$year
  ndata <- length(nt.obs)

  ####### if we are not adding to an existing plot####
  ####################################################
  if(!add.to.plot) {
    if(type == "process") {
      nt <- rep(x = NA, times = length(nt.obs))

      plot(nt.obs[-ndata], nt.obs[-1],
           type = "p",
           pch = 21,
           bg = "black",
           xlab = expression(N[t-1]),
           ylab = expression(N[t]),
           xlim = c(0,7000),
           ylim = c(0, 7000),
           las =1,
           yaxs = "i",
           xaxs = "i")
      plot.fitted.process(r, K, N0, ndata)
    }

    if(type == "observation") {
      plot(year,nt.obs,
           type = "p",
           pch = 21,
           bg= "black",
           ylim = c(0, 7000),
           las=1,
           xlab = "Year",
           ylab = "# Harbor Seals")
      plot.fitted.obs(r, K, N1, ndata, year)
      }
  }
  # if adding to an existing plot
  if(add.to.plot) {
    if(type == "process") {
      plot.fitted.process(r, K, N0, ndata)
    }
    if(type == "observation") {
      plot.fitted.obs(r, K, N1, ndata, year)
    }
  }

}

plot.fitted.process <- function(r, K = NULL, N0, ndata) {
  nt <- rep(x = NA, times = ndata)
  nlist <- seq(from = 0, to = 7000, by = 50)
  if (is.null(K))
    lines(nlist, (1+r)*nlist,
          lwd = 2,
          col = "blue4")
  if (!is.null(K))
    lines(nlist, nlist + r * nlist * (1 - nlist / K),
          lwd = 2,
          col = "red4")
}

plot.fitted.obs <- function(r, K = NULL, N1, ndata, year) {
  if (is.null(K)) {
    nt <- rep(x = NA, times = ndata)
    nt[1] <- N1
    for (i in 2:ndata)
      nt[i] <- nt[i - 1] * (1 + r)
    lines(year, nt,
          lwd = 2,
          col = "blue4")
  }
  if (!is.null(K)) {
    nt <- rep(x = NA, times = ndata)
    nt[1] <- N1
    for (i in 2:ndata)
      nt[i] <- nt[i - 1] + nt[i - 1] *  r * (1 - nt[i - 1] / K)
    lines(year, nt,
          lwd = 2,
          col = "red4")
  }
}