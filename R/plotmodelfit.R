plotmodelfit <- function(x,
                         r,
                         N0 = NULL,
                         N1 = NULL,
                         K = NULL,
                         type = "Process",
                         add.to.plot = F,
                         diagnostic = F,
                         diagnostic.type = NULL) {

  #' Plot data and fitted model
  #'
  #' @description Makes plot of data and one fitted dynamic model.  Can also add an additional fitted model to an existing plot
  #'
  #' @param x: the data frame containing month and population abundances
  #' @param r: the fitted parameter r
  #' @param N0: the fitted parameter N0 (only used for process error models)
  #' @param N1: the fitted parameter N1 (only used for observation error models)
  #' @param K: the fitted parameer K (only used for density dependent models
  #' @param type: must be either "Process" or "Observation". Density independent model appears as a blue line, Density dependent model appears as red line
  #' @param add.to.plot: if TRUE, then adds fitted model to an existing plot.
  #' @param diagnostic: if TRUE, then plots one of three diagnostics (must include diagnostic type)
  #' @param diagnostic.type: must be either "obs_vs_predicted", "res_vs_predicted", or "res_vs_time"
  #' @examples
  #'
  #' #Plot best fitting process, density independent model with data
  #'
  #' di.mle.pars <- proc.di.fit$par
  #' plotmodelfun(x = voles,
  #' r = di.mle.pars[1],
  #' N0= = di.mle.pars[2])
  #' type = "Process")
  #'
  #' # Add density dependent model fit to existing plot:
  #'
  #' dd.mle.pars <- proc.dd.fit$par
  #' plotmodelfun(x = voles,
  #' r = dd.mle.pars[1],
  #' K = dd.mle.pars[2],
  #' N0 = dd.mle.pars[3]
  #' type = "Process",
  #' add.to.plot = TRUE)


  ####### Error Checking######################
  #############################################
  type <- tolower(type)
  names(x) <- tolower(names(x))
  if(type == "process" & is.null(N0)) stop("oops! you must provide N0 to plot the Process error model")

  if(type == "observation" & is.null(N1)) stop("oops! you must provide N1 to plot the Observation error model")

  if(!type == "process" & !type == "observation") stop("oops! model type must be either 'Observation' or 'Process'")
  ########### done error checking##############
  #############################################

  nt.obs <- x$abundance
  month <- x$month
  ndata <- length(nt.obs)
  maxnt <- max(nt.obs)

  xmax <- ymax <- max(pretty(1:maxnt))

  # instructions for diagnostic plots
  if (diagnostic) {
    if(type == "process") plot.diagnostic.process(x, r = r, K = K, N0 = N0, diagnostic.type, log.diagnostic = FALSE)
    if(type == "observation") plot.diagnostic.obs(x, r = r, K = K, N1 = N1, diagnostic.type, log.diagnostic = FALSE)
  }
  # If not a diagnostic plot
  if(!diagnostic) {
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
             xlim = c(0,xmax),
             ylim = c(0, ymax),
             las =1,
             yaxs = "i",
             xaxs = "i")
        plot.fitted.process(r, K, N0, ndata, ymax)
        lines(nt.obs[-ndata], nt.obs[-1],
              col = "gray"
        )
      }

      if(type == "observation") {
        plot(month,nt.obs,
             type = "p",
             pch = 21,
             bg= "black",
             ylim = c(0, ymax),
             las=1,
             xlab = "month",
             ylab = "# Observed")
        plot.fitted.obs(r, K, N1, ndata, month)
      }

    }
    # if adding to an existing plot
    if(add.to.plot) {
      if(type == "process") plot.fitted.process(r, K, N0, ndata, ymax)

      if(type == "observation") plot.fitted.obs(r, K, N1, ndata, month)

    }
  }

}

plot.fitted.process <- function(r, K = NULL, N0, ndata, ymax) {
  nt <- rep(x = NA, times = ndata)
  nlist <- seq(from = 0, to = ymax, length.out = 100)
  if (is.null(K))
    lines(nlist, (1+r)*nlist,
          lwd = 2,
          col = "blue4")
  if (!is.null(K))
    lines(nlist, nlist + r * nlist * (1 - nlist / K),
          lwd = 2,
          col = "red4")
}

plot.fitted.obs <- function(r, K = NULL, N1, ndata, month) {
  if (is.null(K)) {
    nt <- rep(x = NA, times = ndata)
    nt[1] <- N1
    for (i in 2:ndata)
      nt[i] <- nt[i - 1] * (1 + r)
    lines(month, nt,
          lwd = 2,
          col = "blue4")
  }
  if (!is.null(K)) {
    nt <- rep(x = NA, times = ndata)
    nt[1] <- N1
    for (i in 2:ndata)
      nt[i] <- nt[i - 1] + nt[i - 1] *  r * (1 - nt[i - 1] / K)
    lines(month, nt,
          lwd = 2,
          col = "red4")
  }
}




plot.diagnostic.process <- function(x, r, K = NULL, N0, diagnostic.type, log.diagnostic = FALSE) {
  diagnostic.type <- tolower(diagnostic.type)
  # error checking
  if (!diagnostic.type %in% c("obs_vs_predicted", "res_vs_predicted", "res_vs_time")) {
    stop("oops! diagnostic.type must be one of the three allowed strings.  Type ''help(plotmodelfuns)'")
  }

  # Get predicted values
  nt <- x$abundance
  time <- x$month
  ntminus1 <- c(N0, nt[-length(nt)])
  if (is.null(K))  predicted <- (1 + r)* ntminus1
  if (!is.null(K))  predicted <- ntminus1 + r * ntminus1 * (1 - ntminus1 / K)


  if(!log.diagnostic) {
    if (diagnostic.type == "obs_vs_predicted") {
      plot(x = predicted, y = nt, type = "p", pch = 21, bg = "black",
           xlab = "Predicted",
           ylab = "Observed",
           las = 1)
      abline(a=0, b =1, lwd = 2)
    }
    if (diagnostic.type == "res_vs_predicted") {
      plot(x = predicted, y = nt - predicted, type = "p", pch = 21, bg = "black",
           xlab = "Predicted",
           ylab = "Observed - Predicted",
           las = 1)
      abline(h = 0, lwd = 2)
    }
    if (diagnostic.type == "res_vs_time") {
      plot(x = time, y = nt - predicted, type = "p", pch = 21, bg = "black",
           xlab = "Time (months)",
           ylab = "Observed - Predicted",
           las = 1)
      abline(h = 0, lwd = 2)
    }
  }

  if(log.diagnostic) {
    if (diagnostic.type == "obs_vs_predicted") {
      plot(x = log(predicted), y = log(nt), type = "p", pch = 21, bg = "black",
           xlab = "log(Predicted)",
           ylab = "log(Observed)",
           las = 1)
      abline(a=0, b =1, lwd = 2)
    }
    if (diagnostic.type == "res_vs_predicted") {
      plot(x = log(predicted), y = log(nt / predicted), type = "p", pch = 21, bg = "black",
           xlab = "Predicted",
           ylab = "log(Observed / Predicted)",
           las = 1)
      abline(h = 0, lwd = 2)
    }
    if (diagnostic.type == "res_vs_time") {
      plot(x = time, y = log(nt / predicted), type = "p", pch = 21, bg = "black",
           xlab = "Time (months)",
           ylab = "log(Observed / Predicted)",
           las = 1)
      abline(h = 0, lwd = 2)
    }
  }
}


plot.diagnostic.obs <- function(x, r, K = NULL, N1, diagnostic.type, log.diagnostic = FALSE) {
  diagnostic.type <- tolower(diagnostic.type)
  # error checking
  if (!diagnostic.type %in% c("obs_vs_predicted", "res_vs_predicted", "res_vs_time")) {
    stop("oops! diagnostic.type must be one of the three allowed strings.  Type ''help(plotmodelfit)'")
  }

  # Get predicted values
  nt <- x$abundance
  time <- x$month
  predicted <- rep(NA, length(time))
  predicted[1] <- N1
  if (is.null(K)) K <- 1E100
  for (i in 2:length(time)) predicted[i] <- predicted[i-1] + predicted[i -1] * r * (1 - predicted[i - 1] / K)


  if(!log.diagnostic) {
    if (diagnostic.type == "obs_vs_predicted") {
      plot(x = predicted, y = nt, type = "p", pch = 21, bg = "black",
           xlab = "Predicted",
           ylab = "Observed",
           las = 1)
      abline(a=0, b =1, lwd = 2)
    }
    if (diagnostic.type == "res_vs_predicted") {
      plot(x = predicted, y = nt - predicted, type = "p", pch = 21, bg = "black",
           xlab = "Predicted",
           ylab = "Observed - Predicted",
           las = 1)
      abline(h = 0, lwd = 2)
    }
    if (diagnostic.type == "res_vs_time") {
      plot(x = time, y = nt - predicted, type = "p", pch = 21, bg = "black",
           xlab = "Time (months)",
           ylab = "Observed - Predicted",
           las = 1)
      abline(h = 0, lwd = 2)
    }
  }

  if(log.diagnostic) {
    if (diagnostic.type == "obs_vs_predicted") {
      plot(x = log(predicted), y = log(nt), type = "p", pch = 21, bg = "black",
           xlab = "log(Predicted)",
           ylab = "log(Observed)",
           las = 1)
      abline(a=0, b =1, lwd = 2)
    }
    if (diagnostic.type == "res_vs_predicted") {
      plot(x = log(predicted), y = log(nt / predicted), type = "p", pch = 21, bg = "black",
           xlab = "Predicted",
           ylab = "log(Observed / Predicted)",
           las = 1)
      abline(h = 0, lwd = 2)
    }
    if (diagnostic.type == "res_vs_time") {
      plot(x = time, y = log(nt / predicted), type = "p", pch = 21, bg = "black",
           xlab = "Time (months)",
           ylab = "log(Observed / Predicted)",
           las = 1)
      abline(h = 0, lwd = 2)
    }
  }
}

plot.fitted.obs <- function(r, K = NULL, N1, ndata, month) {
  if (is.null(K)) {
    nt <- rep(x = NA, times = ndata)
    nt[1] <- N1
    for (i in 2:ndata)
      nt[i] <- nt[i - 1] * (1 + r)
    lines(month, nt,
          lwd = 2,
          col = "blue4")
  }
  if (!is.null(K)) {
    nt <- rep(x = NA, times = ndata)
    nt[1] <- N1
    for (i in 2:ndata)
      nt[i] <- nt[i - 1] + nt[i - 1] *  r * (1 - nt[i - 1] / K)
    lines(month, nt,
          lwd = 2,
          col = "red4")
  }
}
