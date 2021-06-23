fig2.5 <- function(viewcode = F) {
  #' Other Density Dependent Models
  #'
  #'
  #' Generate figure 2.5 that illustrates four types of density dependent models, with plots showing N at time t+1 vs. N at time t.
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #'
  #'
  #' @export
  #'
  #' @examples
  #' # generate plot
  #' fig2.5()
  #'  #view code
  #' fig2.5(viewcode = TRUE)

K <- 100
# set graphic parameters
reset_graphics_par()
par(mfrow = c(2,2))
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
if(viewcode) cat('
# make K = 100 for all models
K <- 100
nlist <- seq(0, 200, length.out = 500)
# set graphics parameters
par(mfrow = c(2,2))
# Ricker
alphal  <- log(3)
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
}
', sep = "\n")
}
