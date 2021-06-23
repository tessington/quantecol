fig2.6and7 <- function(viewcode = F) {
  #' Compare logistic to Allee effect model
  #'
  #' Generate figure 2.6 and 2.7, comparing standard compensatory model (logistic) with one that has depensatory population growth / Allee Effects
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #' @export
  #'
  #' @examples
  #' # generate plot
  #' fig2.6and7()
  #' #view plotting code
  #' fig2.6and7(viewcode = TRUE)
# code to compare compensatory vs. depensatory models
r <- 0.5
K <- 1000
A <- 200

log.function <- function(n, r, K) r* (1-n/K)
allee.function <- function(n, r, K,A) r*(1-n/K)*(n.list/K-A/K)

n.list <- 0:1000
logistic <-log.function(n.list, r, K)
allee <- allee.function(n.list, 1.5*r, K, A)
# reset graphic parameters to default
reset_graphics_par()
plot(n.list,
     logistic,
     type = "l",
     lwd =3,
     col = "gray50",
     xlab = "Population Size",
     ylab ="f(N)",
     xlim = c(0, K),
     ylim = c(-0.20, 0.5),
     las = 1,
     xaxs = "i",
     yaxs = "i",
     cex.axis = 1.25,
     cex.lab = 1.25)

lines(n.list,
      allee,
      lwd = 3,
      col = "black")

abline (h = 0, lty = "dotted", col = "black", lwd =3, xpd = F)


n.list <- 0:1000
par(oma = c(0,2,0,0))
logistic <-n.list * log.function(n.list, r, K)
allee <- n.list * allee.function(n.list, 1.5*r, K, A)

plot(n.list,
     logistic,
     type = "l",
     lwd =3,
     col = "gray50",
     xlab = "Population Size (N)",
     ylab = "",
     xlim = c(0, K),
     ylim = c(-15, 130),
     las = 1,
     xaxs = "i",
     yaxs = "i",
     cex.axis = 1.25,
     cex.lab = 1.25)

mtext(side = 2, text = expression(frac(paste(Delta, N), paste(Delta, t))), line = 3, las =0, cex = 1.5)
lines(n.list,
      allee,
      lwd = 3,
      col = "black")

abline (h = 0, lty = "dotted", col = "black", lwd =3, xpd = F)
if(viewcode) cat('# code to compare compensatory vs. depensatory models
r <- 0.5
K <- 1000
A <- 200

# create functions to calculate per capita growth rate for the two models
log.function <- function(n, r, K) r* (1-n/K)
allee.function <- function(n, r, K,A) r*(1-n/K)*(n.list/K-A/K)

n.list <- 0:1000
logistic <-log.function(n.list, r, K)
allee <- allee.function(n.list, 1.5*r, K, A)
plot(n.list,
     logistic,
     type = "l",
     lwd =3,
     col = "gray50",
     xlab = "Population Size",
     ylab ="f(N)",
     xlim = c(0, K),
     ylim = c(-0.20, 0.5),
     las = 1,
     xaxs = "i",
     yaxs = "i",
     cex.axis = 1.25,
     cex.lab = 1.25)

lines(n.list,
      allee,
      lwd = 3,
      col = "black")

abline (h = 0, lty = "dotted", col = "black", lwd =3, xpd = F)


n.list <- 0:1000
# set graphics parameters
par(oma = c(0,2,0,0))
logistic <-n.list * log.function(n.list, r, K)
allee <- n.list * allee.function(n.list, 1.5*r, K, A)

plot(n.list,
     logistic,
     type = "l",
     lwd =3,
     col = "gray50",
     xlab = "Population Size (N)",
     ylab = "",
     xlim = c(0, K),
     ylim = c(-15, 130),
     las = 1,
     xaxs = "i",
     yaxs = "i",
     cex.axis = 1.25,
     cex.lab = 1.25)

mtext(side = 2, text = expression(frac(paste(Delta, N), paste(Delta, t))), line = 3, las =0, cex = 1.5)
lines(n.list,
      allee,
      lwd = 3,
      col = "black")

abline (h = 0, lty = "dotted", col = "black", lwd =3, xpd = F)
', sep = "\n")
}
