fig2.2 <- function() {
  #' Density Independent Model Dynamics
  #'
  #'Generates Figure 2.2, showing three types of model dynamics of the density independent model
  #'
  #' @export
  #'
  #' @examples
  #' #view plotting commands
  #' print(fig2.2)
  #' # generate plot
  #' fig2.2()

# reset graphic parameters to default
reset_graphics_par()

lambda=1.05
years <-0:20

N.t.105<-10*lambda^years

lambda=0.95
N.t.095<-10*lambda^years


xlims<-c(0,20)
ylims<-c(0,30)
par(las = 1)
plot(years,
  N.t.105,
  type = "l",
  col = "black",
  lwd = 2,
  xlab = "Time",
  ylab = expression(paste("N"["t"])),
  xlim = xlims,
  ylim = ylims
)
lines(years, N.t.095,
      col = "black",
      lwd = 2)
abline(h = 10,
       col = "black",
       lwd = 2) # this adds horizontal line associated with lambda = 1

text(x = 10, y= 17, labels = parse(text=expression(paste(lambda,">1",sep=""))))
text(x = 10, y= 11, labels = parse(text=expression(paste(lambda,"=1",sep=""))))
text(x = 10, y= 5, labels = parse(text=expression(paste(lambda,"<1",sep=""))))

}
