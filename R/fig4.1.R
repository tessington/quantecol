fig4.1 <- function(stability = "stable") {
  #' Generate figure 4.1: What is model stability?
  #'
  #' This function uses a continuous-time lotka-volterra competition model to introduce the concept of stability, how to use the Adams-Bashford method to simulate a model with differential equations, and the calculation of Jacobian matrices and dominant eigenvalue.  The user can ask for a "stable" model configuration or an "unstable" model configuration
  #'
  #' @param stability character string that is either "stable" or "unstable"
  #' @return a list object containing jacobian matrix and dominant eigenvalue
  #' @export
  #'
  #' @examples
  #' #view  commands
  #' print(fig4.2)
  #' # generate plot with a stable model
  #' fig2.2("stable")
  #' # generate a plot with an unstable model
  #' fig2.2("unstable")



# function to run the Adams - Bashford method to simulate the differential equations
run.dxydt<-function(Xstart,
                    Ystart,
                    parslist,
                    t.perturb = 20,
                    perturb = c(1.05,1),
                    n.years = 100,
                    deltat = 0.1) {
  rx <- parslist$rx
  Kx<-parslist$Kx
  alpha<-parslist$alpha
  ry <- parslist$ry
  Ky <- parslist$Ky
  beta<-parslist$beta

  timelist <- seq(0, n.years, by = deltat)
  n.steps<-length(timelist)

  # Functions to calculate the value of the dX/dt or dY/dt
  dxdt<-function(X,Y,rx,Kx,alpha) rx*X*(1-(X+alpha*Y)/Kx)
  dydt<-function(X,Y,ry,Ky,beta) ry*Y*(1-(Y+beta*X)/Ky)

  # set up output matrix.
  output<-matrix(NA,nrow=n.steps,ncol=2)
  # set up matrix to hold values of dX/dt and dY/dt at time step t
  dXYdt.t<-matrix(NA,nrow=n.steps,ncol=2)

  # initialize with starting conditions
  output[1,1:2]<-c(Xstart, Ystart)
  dXYdt.t[1,1]<-dxdt(Xstart,Ystart,rx,Kx,alpha)
  dXYdt.t[1,2]<-dydt(Xstart,Ystart,ry,Ky,beta)
  # use the Euler method for first time step
  output[2,1]<-output[1,1]+deltat*dXYdt.t[1,1]
  output[2,2]<-output[1,2]+deltat*dXYdt.t[1,2]

  # now use adams bashford
  for (t in 3:n.steps){
    dxdt.t.minus1 <- dXYdt.t[t-2,1]
    dydt.t.minus1 <- dXYdt.t[t-2,2]
    X.t <- output[t-1,1]
    Y.t <- output[t-1,2]
    # if this is the year of the perturbation, then adjust X and Y accordingly
    if(timelist[t] == t.perturb) {
      X.t <- X.t * perturb[1]
      Y.t <- Y.t * perturb[2]
    }

    dxdt.t <- dxdt(X.t, Y.t, rx, Kx, alpha)
    dydt.t <- dydt(X.t, Y.t, ry, Ky, alpha)
    Xt.plus1 <- X.t +  deltat /2 * (3*dxdt.t - dxdt.t.minus1[1])
    Yt.plus1 <- Y.t +  deltat /2 * (3*dydt.t - dydt.t.minus1[1])
    output[t,] <- c(Xt.plus1, Yt.plus1)
    dXYdt.t[t-1,] <- c(dxdt.t, dydt.t)
  }

  return(output)

}

# set up parameters
rx<-0.5
ry<-0.5

if(stability == "stable") {
  alpha <- 0.75
  beta <- 0.75
  Kx <- 87.75
  Ky <- 87.25
}
if(stability == "unstable") {
  alpha <- 1.25
  beta <- 1.25
  Kx <- 112.25
  Ky <- 112.75
}

parslist <- list(rx = rx, Kx = Kx, ry = ry, Ky = Ky, alpha = alpha, beta = beta)

# year of the perturbation
t.perturb=20
# how many years to simulate
n.years<-100


# Get Equilibrium
Xstar <- (Kx - alpha * Ky) / (1 - alpha * beta)
Ystar <- (Ky - beta*Kx) / (1 - alpha * beta)
# Set starting values at equilibrium
Xstart=Xstar
Ystart=Ystar

# call the run.dxydt function to get the model dynamics
output<-run.dxydt(Xstart,Ystart,parslist, t.perturb=t.perturb,perturb=c(1.05,1),n.years = n.years,
                 deltat = deltat)

# plot the output
reset_graphics_par()
plot((1:nrow(output)) * deltat,
     output[, 1],
     type = "l",
     col = "black",
     lwd = 3,
     xlab = "",
     ylab = "",
     ylim = c(0, 110),
     xaxs = "i",
     axes = "F"
)
box()
mtext(side = 1, line = 1, text = "Time")
mtext(side = 2, line = 1, text = "State Variable", las = 0)
lines(1:nrow(output)*deltat,
      output[,2],
      type = "l",
      lwd = 3,
      col = "gray50")

# calculate Jacobian Matrix
axx <- rx - 2 * rx * Xstar / Kx - rx * alpha * Ystar/Kx
axy <- -rx * alpha * Xstar / Kx
ayy <- ry - 2 * ry * Ystar / Ky - ry * beta * Xstar/Ky
ayx <- -ry * beta * Ystar / Ky

A <- matrix(c(axx, axy, ayx, ayy), byrow = T, nrow = 2, ncol= 2)
ev <- eigen(A)$values
# find maximum real part of eigenvalue
real.ev <- max(Re(ev))
dom.eigenvalue <- ev[Re(ev)==real.ev]

return(list(Jacobian = A, eigenvalue = dom.eigenvalue))


}