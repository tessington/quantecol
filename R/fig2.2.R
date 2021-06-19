fig2.2 <- function(viewcode = F) {
  #' Density Independent Model Dynamics
  #'
  #' Generates Figure 2.2, showing three types of model dynamics of the density independent model
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #' @export
  #'
  #' @examples
  #' # generate plot
  #' fig2.2()
  #' #view plotting code
  #' fig2.2(viewcode = T)

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

if(viewcode) cat(readLines(con = "txt/fig2.2.txt"), sep = "\n")
}
