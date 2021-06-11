nicholson_bailey <- function(perturb = 1.01) {
  #' Nicholson-Bailey Host Parasitoid Model
  #'
  #' This function simulates the dynamics of the Nicholson Bailey Host Parasitoid model, plots Fig. 4.9, and returns the Jacobian matrix, dominant eigenvalue and eigenvalue magnitude
  #'
  #' @param perturb is a multiplier that is used to perturb the model away from equilibrium.  The default 1.01 perturbs by 1 percent away from equilibrium
  #' @return a list object containing jacobian matrix, dominant eigenvalue, and eigenvalue magnitude
  #' @export
  #'
  #' @examples
  #' #view  commands
  #' print(nicholson_bailey)
  #' # generate plot with equivalent to Fig 4.9
  #' nicholson_bailey()
  #' # generate a plot with larger perturbation away from equilibrium
  #' nicholson_bailey(perturb = 1.05)

# Parameter values
lambda <- 1.1
a <- 0.2
c <- 0.75

# calculate equilibrium host and parasitoid values
Nstar <- log(lambda) / (a * c * (1 - lambda ^ (-1)))
Pstar <- log(lambda) / a


# Set starting Values
Nstart <- Nstar * perturb
Pstart <- Pstar * perturb


# Plot isoclines
# scale axes
Pmax <- 6 * Pstar
Nmax <- 2 * Nstar
# create lists of N and P for isocline calculation
Plist = as.matrix(seq(0.1, Pmax, length.out = 25))
Nlist = as.matrix(seq(0, Nmax, length.out = 25))
# create empty matrices to hold isoclines
Pisocline = matrix(NA, 25, 2)
Nisocline = matrix(NA, 25, 2)

# cycle through either Plist or Nlist, calculate isoclines for host and parasitoids
for (i in 1:25) {
  Ptemp <- Plist[i]
  Ntemp <- Ptemp / (c * lambda * (1 - exp(-a * Ptemp)))
  Pisocline[i, 1:2] = c(Ntemp, Ptemp)
  Nisocline[i, 1:2] = c(Nlist[i], Pstar)
}

# set up simulation
# number of years
niters <- 150
# output to hold the results
output <- matrix(NA, niters, 2)
output[1, 1] <- Nstart
output[1, 2] <- Pstart

for (i in 2:niters) {
  Nt <- output[i - 1, 1]
  Pt <- output[i - 1, 2]
  Nt.plus1 <- lambda * Nt * (exp(-a * Pt))
  Pt.plus1 <- c * lambda * Nt * (1 - exp(-a * Pt))
  output[i, 1:2] <- c(Nt.plus1, Pt.plus1)
}

# set up plotting parameters
Pmaxplot = ceiling(Pmax)
Nmaxplot = ceiling(Nmax)

reset_graphics_par()
# plot time dynamics
plot(1:60,output[1:60, 1],
     col = "black",
     lwd = 3,
     type = "l",
     ylim = c(0, Nmaxplot),
     xlab = "",
     ylab = "",
     axes = F)
box()
mtext(text="Year",side=1,line=1,cex=1)
mtext(text="Population Size",side=2,line=1,cex=1)

lines(1:60, output[1:60, 2],
      col = "gray70",
      lwd = 3)

# plot the isoclines with trajectory overlayed
plot(
  Nisocline[, 1],
  Nisocline[, 2],
  type = 'l',
  col = "black",
  ylab = "",
  xlab = "",
  xlim = c(0, Nmaxplot),
  ylim = c(0, Pmaxplot),
  axes = F,
  lwd = 3,
  xaxs = "i",
  yaxs = "i"
)
mtext(text="Host",side=1,line=,cex=1)
mtext(text="Parasitoid",side=2,line=1,cex=1)

box()
lines(Pisocline[, 1], Pisocline[, 2], lwd = 3, col = "gray70")
lines(output[1:60, 1], output[1:60, 2], col = "Black", lwd = 3)

# Calculate Jacobian Matrix
Jacob <- matrix(0, 2, 2)
Jacob[1, 1] <- lambda * exp(-a * Pstar)
Jacob[2, 1] <- -a * Nstar * lambda * exp(-a * Pstar)
Jacob[1, 2] <- c * (1 - exp(-a * Pstar))
Jacob[2, 2] <- c * Nstar * a * exp(-a * Pstar)

ev = eigen(Jacob)

eigen.magnitude = (Re(ev$values[1]) ^ 2 + Im(ev$values[1]) ^ 2) ^ 0.5
dominant.eigenvalue <- ev$values[1]

return(list(Jacobian = Jacob, eigenvalue = dominant.eigenvalue, eigenvalue_magnitude = eigen.magnitude))
}
