fig2.5 <- function() {
  #' Generate figure 2.5: Other Density Dependent models
  #'
  #'
  #' @export
  #'
  #' @examples
  #' #view plotting commands
  #' print(fig2.5)
  #' # generate plot
  #' fig2.5()

K <- 100
# reset graphic parameters to default
reset_graphics_par()

par(mfrow = c(2,2), mar = c(4,4.5,1,1), xpd = F)
# Ricker
alphal  <- log(3)
nlist <- seq(0, 200, length.out = 500)

plot(nlist,nlist * exp(alphal*(1-nlist/K)),
     type = "l",
     lwd=2,
     xlab =expression(paste("N"["t"])),
     ylab = expression(paste("N"["t+1"])),
     las = 1,
     xlim <- c(0, 200),
     xaxs = "i",
     ylim <- c(0,150),
     yaxs = "i",
     cex.lab = 1.25,
     axes = F)
box()
axis(1, at = c(0, 100, 200))
axis(2, at = c(0, 50, 100),las=1)
text(x = 20, y =130, "Ricker", pos = 4)

# Beverton Holt
# set beta so that has correct carrying capacity
alpha <- exp(alphal)
beta = (alpha - 1)/K

plot(nlist,nlist * alpha/(1+nlist*beta)-nlist+nlist,
     type = "l",
     lwd=2,
     xlab =expression(paste("N"["t"])),
     ylab = expression(paste("N"["t+1"])),
     las = 1,
     xlim <- c(0, 200),
     xaxs = "i",
     ylim <- c(0,150),
     yaxs = "i",
     cex.lab = 1.25,
     axes = F)
box()
axis(1, at = c(0, 100, 200))
axis(2, at = c(0, 50, 100),las=1)
text(x = 20, y =130, "Beverton-Holt", pos = 4)


# Gompertz
b <- - 0.5
plot(nlist,nlist *(nlist/K)^b,
     type = "l",
     lwd=2,
     xlab =expression(paste("N"["t"])),
     ylab = expression(paste("N"["t+1"])),
     las = 1,
     xlim <- c(0, 200),
     xaxs = "i",
     ylim <- c(0,150),
     yaxs = "i",
     cex.lab = 1.25,
     axes = F)
box()
axis(1, at = c(0, 100, 200))
axis(2, at = c(0, 50, 100),las=1)
text(x = 20, y =130, "Gompertz", pos = 4)

# Theta- Logistic
r <- 0.5
theta <- 0.75
plot(nlist,nlist * r * (1-(nlist/K)^theta)+nlist,
     type = "l",
     lwd=2,
     xlab =expression(paste("N"["t"])),
     ylab = expression(paste("N"["t+1"])),
     las = 1,
     xlim <- c(0, 200),
     xaxs = "i",
     ylim <- c(0,150),
     yaxs = "i",
     cex.lab = 1.25,
     axes = F)
box()
axis(1, at = c(0, 100, 200))
axis(2, at = c(0, 50, 100),las=1)
theta <- 1.5
lines(nlist,nlist * r * (1-(nlist/K)^theta)+nlist,
      type = "l",
      lwd=2)
text(x = 20, y =130, "theta-logistic", pos = 4)
settings::reset_par()
}
