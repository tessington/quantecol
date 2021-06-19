lotkaVolterra <- function(perturb = 1.05, viewcode = FALSE) {
  #' Simulate Lotka-Volterra Predator-Prey Model
  #'
  #' Simulate the time dynamics of the Lotka-Volterra model, and recreate figure 4.8
  #' @param  perturb is a multiplier that is used to perturb the model away from equilibrium.  The default 1.05 perturbs by 5 percent away from equilibrium
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #' @return a list containing the jacobian matrix and dominant eigenvalue, and creates figure 4.8
  #' @export
  #' @examples
  #' # Recreate figure 4.8 exactly
  #' lotkaVolterra(perturb = 1.05)
  #' # create a larger initial perturbation away from equilibrim
  #' lotkaVolterra(perturb = 1.25)
  #' # View the code
  #' lotkaVolterra(viewcode = TRUE)


  # Function that calculates the values of the dN/dt and dP/dt
  dXdt.fun <- function (modelpars, X) {
    list2env(modelpars, envir = environment())
    dXdt <- matrix(NA, nrow = length(X), ncol = 1)
    dXdt[1] <- a * X[1] - b * X[1] * X[2]
    dXdt[2] <- c * b * X[1] * X[2] - d * X[2]
    return(dXdt)
  }
  # specify model parameters
  a=.4
  b=.5
  c=.3
  d=.3
  # set up isocline and equilibrium values
  # N.list is  a vector containing different values of N.
  N.list <- seq(0, 5, length.out = 100)
  # equilibrium densities of predator and prey
  Pstar <- a / b
  Nstar <- d / (c * b)
  # create isoclines  (Niso = prey isocline, P iso = predator isosline) as two columns, where the first column is N, the second column is P
  # this is all a little bit overkill because the isoclines here are either vertical or horizontal lines.
  Niso <- cbind(N.list, rep(Pstar, times = length(N.list)))
  Piso <- cbind(c(Nstar, Nstar), c(0, 5))

  # set up objects to simulate model dynamics
  modelpars <- list(a = a,
                    b = b,
                    c = c,
                    d = d)
  # how many years
  n.years <- 50
  # time increment to use
  deltat <- 0.1


  # call the runge.kutta routine, with starting values of state values perturbed by multiplier "perturb".
  output <- runge.kutta(
    c(perturb * Nstar, perturb * Pstar),
    modelpars = modelpars,
    fun = dXdt.fun,
    deltat = deltat,
    n.years = n.years
  )

  # assign column names
  colnames(output) <- c("Time", "Prey", "Predator")

  # plot the results as state variable vs. time
  # assign colors for plot
  PreyCol <- "black"
  PredCol <- "gray70"

  ylim <- c(0, 3)
  reset_graphics_par()
  par(
    mfrow = c(1, 2),
    mar = c(2, 2, 1, 1),
    xpd = FALSE,
    las = 0,
    new = F
  )
  plot(
    output[, "Time"],
    output[, "Prey"],
    type = "l",
    lwd = 3,
    col = PreyCol,
    ylab = "",
    xlab = "",
    axes = F,
    ylim = ylim
  )
  lines(output[, "Time"], output[, "Predator"], lwd = 3, col = PredCol)
  mtext(
    text = "Year",
    side = 1,
    line = 1,
    cex = 1
  )
  mtext(
    text = "Population Size",
    side = 2,
    line = 1,
    cex = 1
  )
  box()

  # Plot the results as a state-space plot
  plot(
    output[, 2],
    output[, 3],
    type = "l",
    lwd = 3,
    col = "black",
    xlab = "",
    ylab = "",
    axes = F,
    xlim = c(0, 4),
    ylim = c(0, 1.6),
    xaxs = "i",
    yaxs = "i"
  )
  #  add a box around the plot
  box()
  # add isoclines
  lines(Niso[, 1], Niso[, 2], lwd = 3, col = PreyCol)
  lines(Piso[, 1], Piso[, 2], lwd = 3, col = PredCol)
  # another way to add axis labels, using "mtext" which stands for "margin text"
  mtext(
    text = "Prey",
    side = 1,
    line = 1,
    cex = 1
  )
  mtext(
    text = "Predator",
    side = 2,
    line = 1,
    cex = 1
  )

  # calculate jacobian matrix
  A <- matrix(NA, nrow = 2, ncol = 2)
  A[1, 1] <- 0
  A[1, 2] <- -d / c
  A[2, 1] <- a * c
  A[2, 2] <- 0

  ev <- eigen(A)$values
  max.real.eigen <- max(Re(ev))
  eigenvalue <- ev[Re(ev) == max.real.eigen][1]
  if(viewcode) cat(readLines(con = "txt/lotkaVolterra.txt"), sep = "\n")

  return(list(Jacobian = A, eigenvalue = eigenvalue))
}
