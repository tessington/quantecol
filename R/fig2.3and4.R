fig2.3and4 <- function() {
  #' The structure of the Logistic Model
  #'
  #' Generate figures 2.3 and 2.4 showing the assumed linear relationship between per-capita growth rate and population density, and the resulting relationship between Delta N Delta t and Population size.
  #'
  #'
  #' @export
  #'
  #' @examples
  #' #view plotting commands
  #' print(fig2.3and4)
  #' # generate plot
  #' fig2.3and4()
  # first generate Figure 2.3
  # specify parameters
  r = 0.5
  K = 100

  Nlist <- 0:200
  # close whatever plotting device is open so that we can start a new one
  # reset graphic parameters to default
  reset_graphics_par()

 # par <- settings::reset_par()
  par(las = 1)

  log_fun <- function(x, r, K) r * (1 - x / K)

  plot(
    Nlist,
    log_fun(Nlist, r, K),
    type = "l",
    lwd = 2,
    col = "black",
    xlim = c(min(Nlist), max(Nlist)),
    ylim = c(-r * 1.1, r * 1.1),
    ylab = "",
    xlab = "N",
    yaxs = "i",
    xaxs = "i",
    cex.axis = 1.0,
    cex.lab = 1.0
  )
  abline(h = 0, xpd = F, lty = "dotted")
  par(las = 0)
  mtext(
    side = 2,
    line = 4,
    cex = 1,
    text = "f(N)"
  )
  text(
    x = 10,
    y = 0.515,
    labels = "r",
    cex = 1
  )
  text(
    x = 102,
    y = 0.05,
    labels = "K",
    cex = 1
  )
  text(
    x = 55,
    y = 0.25,
    labels = expression(slope == -beta),
    cex = 1,
    pos = 4
  )
  dev.off()

  # plot figure 2.4
  plot.new()
  Nlist <- 0:100
  log.plot <- Nlist * r * (1 - Nlist / K)
  par(las = 1, mar = c(5,7,5,5), mfrow = c(1,2))

  plot(
    Nlist,
    log.plot,
    type = "l",
    col = "black",
    lwd = 2,
    cex = 1.5,
    xlab = "N",
    ylab = "",
    xlim = c(0, 100),
    xaxs = "i",
    yaxs = "i",
    cex.axis = 1,
    cex.lab = 1
  )
  par(las = 0)

  mtext(
    side = 2,
    line = 2,
    text = expression(frac(Delta ~ N, Delta ~ t))
  )
  par(las = 1)
  Nt <- 0:200
  Ntplus1 <- Nt + r * Nt * (1 - Nt /K)
  plot(
    Nt,
    Ntplus1,
    type = "l",
    col = "black",
    lwd = 2,
    cex = 1.5,
    xlab = expression(paste("N"["t"])),
    ylab = expression(paste("N"["t"+1])),
    xlim = c(0, 200),
    xaxs = "i",
    yaxs = "i",
    ylim = c(0, 115),
    cex.axis = 1,
    cex.lab = 1.35
  )


}

