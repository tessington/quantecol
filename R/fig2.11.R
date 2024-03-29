fig2.11 <- function(r = 0.5, viewcode = F) {
  #' Dynamic behavior of the logistic model
  #'
  #'
  #' Generate figures 2.11 through 2.16 by specifying the maximum rate of population growth (r), and seeing accompanying cobweb diagram and the plot of population abundance vs. time
  #'
  #' @param r maximum rate of population growth
  #' @param viewcode FALSE to run function (default), TRUE to run function and print code
  #'
  #' @export
  #'
  #' @examples

  #' # generate plot with "typical" dynamics
  #' fig2.11(r = 0.5)
  #' # generate plot with "complex" dynamics
  #' fig2.11(r = 2.1)
  #' #view plotting commands
  #' print(viewcode = T)


K=100
n.years<-25
nstart <- 20

# run  model
output <- rep(NA, times = n.years)
output[1] <- nstart
for (i in 2:n.years) {
  nt <- output[i-1]
  ntplus1 <-nt + r * nt * (1 - nt /K)
  output[i] <- ntplus1
}
# reset graphic parameters to default
reset_graphics_par()
par(las = 1, mar = c(5, 5, 5, 1), oma = c(1,5,1,1))
layout(matrix(c(1,1,2),nrow = 1, ncol=3))
modelpars = list(r = r, K = K)
plotCobweb(modeloutput = output, modelpars = modelpars, modelname = "logistic")


# plot time dynamic plot on right

plot(1:n.years, output,
     type = "l",
     lwd=3,
     col ="black",
     axes =F,
     ylab = "",
     xlab = 'Time',
     xaxs = "i",
     yaxs = "i",
     ylim = c(0, 1.5*K),
     xlim = c(1, n.years),
     cex.lab = 1.5
)
box()
axis(1, at = c(1, 25, 50), cex.axis = 1.25)

if (viewcode) cat('# Set initialization parameters
K=100
n.years<-25
nstart <- 20

# run  model
output <- rep(NA, times = n.years)
output[1] <- nstart
for (i in 2:n.years) {
  nt <- output[i-1]
  ntplus1 <-nt + r * nt * (1 - nt /K)
  output[i] <- ntplus1
}
# set up graphics parameters to make plot
par(las = 1, mar = c(5, 5, 5, 1), oma = c(1,5,1,1))
layout(matrix(c(1,1,2),nrow = 1, ncol=3))
# make a list called modelpars
modelpars = list(r = r, K = K)
# run function "plotCobweb".  see help(plotCobweb) for details)
plotCobweb(modeloutput = output, modelpars = modelpars, modelname = "logistic")


# plot time dynamic plot on right
plot(1:n.years, output,
     type = "l",
     lwd=3,
     col ="black",
     axes =F,
     ylab = "",
     xlab = "Time",
     xaxs = "i",
     yaxs = "i",
     ylim = c(0, 1.5*K),
     xlim = c(1, n.years),
     cex.lab = 1.5
)
box()
axis(1, at = c(1, 25, 50), cex.axis = 1.25)', sep = "\n")
}
