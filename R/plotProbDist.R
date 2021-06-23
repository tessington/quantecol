plotProbDist <-function(plottype ="binomial", viewcode = FALSE) {
  #' View Probability Distributions
  #'
  #'
  #' Compare probability-mass and -density functions, as illustrated in Figures 7.1 - 7.6.
  #'
  #' @param plottype a character string that is either
  #' \itemize{
  #' \item 'binomial'
  #' \item 'poisson'
  #' \item 'negativebinomial'
  #' \item 'normal'
  #' \item 'gamma'
  #' \item 'studentst'
  #' }
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #' @export
  #'
  #' @examples
  #' # Generate figure 7.1
  #' plotProbDist("binomial")
  #' # View all plotting code
  #' plotProbDist(viewcode = TRUE)


  # error checking
  plottype <- tolower(plottype)

  if(!plottype %in% c("binomial",
                      "poisson",
                      "negativebinomial",
                      "normal",
                      "gamma",
                      "studentst")) stop("required input parameter plottype should be either 'binomial', poisson', 'negativebinomial', 'normal', 'gamma' or 'studentst'")

if(plottype =="binomial") {

p <- 0.1
n <- 20
k <- 0:20
binom.plot<-function(p,N) {
  xlist<-0:N
  p.N<-dbinom(xlist,size=N,prob=p)
  plot(xlist, p.N,type = "h", xlab="number mature",ylab="Probability",bg="gray",col="black",ylim=c(0,0.3), yaxs = "i", lwd = 2, las = 1)

}
reset_graphics_par()
par(mfrow = c(1,2), oma = c(0,0,1,2),mar = c(5,4,1,0))
binom.plot(p,n)
binom.plot(p = 0.5,n)
}

if(plottype =="poisson") {

poisson.plot<-function(r,t){
  N=max(3,4*r*t)
  xlist<-0:N
  p.N<-dpois(xlist,lambda=r*t)
  plot(xlist, p.N, type = "h", xlab = "number of attacks",ylab="Probability",bg="gray",col="black",ylim=c(0,0.2),xlim = c(0.5, 30.5), yaxs = "i", lwd = 2, las = 1)
}


r<- 0.1
t <- 50
reset_graphics_par()
poisson.plot(r,t)
r <- 0.30
poisson.plot(r,t)
}

if(plottype =="negativebinomial") {

negbin.plot<-function(r,t,n){
  N=50
  xlist<-0:N
  p.N<-dnbinom(xlist, mu = r*t, size = n)

  plot(xlist, p.N, type = "h", xlab="number of attacks",ylab="Probability",bg="gray",col="black",yaxs = "i",ylim=c(0,0.1),xlim=c(-0.5,40.5), cex.axis = 0.8, las = 1)
}



r<-0.3
n <- 25
t<- 50
reset_graphics_par()
par(mfrow = c(1,2), oma = c(0,0,1,2),mar = c(5,4,1,1))
negbin.plot(r,t,n)

n <- 1
negbin.plot(r,t,n)

}

if(plottype == "normal") {
normal.plot<-function(mu,sigma) {
  x.list<-seq(10,90,by=0.25)
  lower.area<-30
  upper.area<-40
  p.x<-dnorm(x.list,mean=mu,sd=sigma)

  plot(x.list,p.x,type="l",lwd=3,col="black",ylab="f(x)",xlab="x",ylim=c(0,.1),yaxs='i',las=1, cex.axis = 0.9)

  x.poly<-seq(lower.area,upper.area,length.out=100)
  p.poly<-dnorm(x.poly,mean=mu,sd=sigma)
  polygon(c(x.poly,rev(x.poly)),c(p.poly,rep(0,length(p.poly))),border="gray50",lwd=0.1,col="gray50")
  text(x=10,y=.085,labels=substitute(sigma==a,list(a=sigma)),pos=4)

}

reset_graphics_par()
par(mfrow = c(1,2), oma = c(0,0,0,0),mar = c(4,4,1,0.5))
normal.plot(mu = 50, sigma = 5)
normal.plot(mu = 50, sigma = 15)
}

if(plottype == "gamma") {

# Compare gamma to log normal

make.plot <- function(mu, sigma) {
  # get log-normal parameters
  mean = exp(mu + sigma^2/2)
  variance <- (exp(sigma ^2)-1)*exp(2 *mu +sigma ^2)
  tmp.x <- rlnorm(1000, meanlog = mu, sdlog = sigma)
  xlist <- seq(0.01, 30, length.out = 1000)
  plot(xlist, dlnorm(xlist, meanlog = mu, sdlog = sigma), type = "l", lwd = 2, col = "black", las = 1, ylim = c(0, 0.35), yaxs = "i", xlim = c(0, 30), xaxs = "i",
       xlab = "x",
       ylab = "Probability Density",cex.axis = 0.75)
  beta <- mean / variance
  alpha <- beta * mean

  f.gamma <- dgamma(xlist, shape = alpha, rate = beta)
  lines(xlist, f.gamma, lwd = 2, col = "blue")
}
reset_graphics_par()
par(mfrow = c(1,2), mar = c(4,4 ,1,1))
sigma.2.use <- 0.55
make.plot(mu = log(5)- sigma.2.use^2/2, sigma = sigma.2.use)
sigma.2.use <- 1.20
make.plot(mu = log(5)-sigma.2.use^2/2, sigma = sigma.2.use)

}

if(plottype == "studentst") {
alpha <- 1.
v <- 3
variance = 7.5
sigma2 <- variance * (v-2) / v
mu <- 20

dgt <- function (x,v, sigma2,mu) gamma((v+1)/2)/(gamma(v/2)*sqrt(v*pi*sigma2)) *
  (1+1/v * ((x-mu)^2/sigma2))^(-(v+1)/2)


xlist <- seq(5,35, by = 0.01)
pgt <- dgt(xlist, v, sigma2, mu)
reset_graphics_par()
par(fig = c(0,1,0,1))
plot(xlist, pgt,
     type = "l",
     lwd = 3,
     col = "gray70",
     xlab = "x",
     ylab = "f(x)",
     las = 1,
     ylim = c(0, 0.25),
     yaxs = "i")

# compare to normal distribution with same variance

pn <- dnorm(xlist, mean = mu, sd = variance^0.5)

lines(xlist, pn,
      lwd = 3,
      col = "black")

par(fig = c(0.05,0.50, 0.3, 0.975), new = T)
xlist.short <- xlist[which(xlist<=14)]
pgt.short <- pgt[which(xlist<=14)]
pn.short <- pn[which(xlist<=14)]
plot(xlist.short, pgt.short,
     type = "l",
     lwd = 3,
     col = "gray70",
     xlab = "",
     ylab = "",
     las =1,
     ylim = c(0, 0.005),
     yaxs = "i",
     axes = F)
box()
axis(1, at = c(6,10,14), cex.axis = 0.8, line = 0.0)
axis(2, at = c(0, 0.005), labels = F, cex.axis = 0.8, las =1)
lines(xlist.short, pn.short,
      lwd = 3,
      col = "black")
}
  if(viewcode) cat('if(plottype =="binomial") {
p <- 0.1
n <- 20
k <- 0:20
binom.plot<-function(p,N) {
  xlist<-0:N
  p.N<-dbinom(xlist,size=N,prob=p)
  plot(xlist, p.N,type = "h", xlab="number mature",ylab="Probability",bg="gray",col="black",ylim=c(0,0.3), yaxs = "i", lwd = 2, las = 1)

}

par(mfrow = c(1,2), oma = c(0,0,1,2),mar = c(5,4,1,0))
binom.plot(p,n)
binom.plot(p = 0.5,n)
}

if(plottype =="poisson") {
# Function to plot poisson probability function
poisson.plot<-function(r,t){
  N=max(3,4*r*t)
  xlist<-0:N
  p.N<-dpois(xlist,lambda=r*t)
  plot(xlist, p.N, type = "h", xlab = "number of attacks",ylab="Probability",bg="gray",col="black",ylim=c(0,0.2),xlim = c(0.5, 30.5), yaxs = "i", lwd = 2, las = 1)
}


r<- 0.1
t <- 50
poisson.plot(r,t)
r <- 0.30
poisson.plot(r,t)
}

if(plottype =="negativebinomial") {
# function to plot negative binomial distribution
negbin.plot<-function(r,t,n){
  N=50
  xlist<-0:N
  p.N<-dnbinom(xlist, mu = r*t, size = n)

  plot(xlist, p.N, type = "h", xlab="number of attacks",ylab="Probability",bg="gray",col="black",yaxs = "i",ylim=c(0,0.1),xlim=c(-0.5,40.5), cex.axis = 0.8, las = 1)
}



r<-0.3
n <- 25
t<- 50

par(mfrow = c(1,2), oma = c(0,0,1,2),mar = c(5,4,1,1))
negbin.plot(r,t,n)

n <- 1
negbin.plot(r,t,n)

}

if(plottype == "normal") {
normal.plot<-function(mu,sigma) {
  x.list<-seq(10,90,by=0.25)
  lower.area<-30
  upper.area<-40
  p.x<-dnorm(x.list,mean=mu,sd=sigma)

  plot(x.list,p.x,type="l",lwd=3,col="black",ylab="f(x)",xlab="x",ylim=c(0,.1),yaxs="i",las=1, cex.axis = 0.9)

  x.poly<-seq(lower.area,upper.area,length.out=100)
  p.poly<-dnorm(x.poly,mean=mu,sd=sigma)
  polygon(c(x.poly,rev(x.poly)),c(p.poly,rep(0,length(p.poly))),border="gray50",lwd=0.1,col="gray50")
  text(x=10,y=.085,labels=substitute(sigma==a,list(a=sigma)),pos=4)
  print(area.curve)
}
par(mfrow = c(1,2), oma = c(0,0,0,0),mar = c(4,4,1,0.5))
normal.plot(mu = 50, sigma = 5)
normal.plot(mu = 50, sigma = 15)
}

if(plottype == "gamma") {

# Compare gamma to log normal
make.plot <- function(mu, sigma) {
  # get log-normal parameters
  mean = exp(mu + sigma^2/2)
  variance <- (exp(sigma ^2)-1)*exp(2 *mu +sigma ^2)
  tmp.x <- rlnorm(1000, meanlog = mu, sdlog = sigma)
  xlist <- seq(0.01, 30, length.out = 1000)
  plot(xlist, dlnorm(xlist, meanlog = mu, sdlog = sigma),
       type = "l",
       lwd = 2,
       col = "black",
       las = 1,
       ylim = c(0, 0.35),
       yaxs = "i",
       xlim = c(0, 30),
       xaxs = "i",
       xlab = "x",
       ylab = "Probability Density",cex.axis = 0.75
    )

  beta <- mean / variance
  alpha <- beta * mean
  f.gamma <- dgamma(xlist, shape = alpha, rate = beta)
  lines(xlist, f.gamma, lwd = 2, col = "blue")
}

par(mfrow = c(1,2), mar = c(4,4 ,1,1))
sigma.2.use <- 0.55
make.plot(mu = log(5)- sigma.2.use^2/2, sigma = sigma.2.use)
sigma.2.use <- 1.20
make.plot(mu = log(5)-sigma.2.use^2/2, sigma = sigma.2.use)

}

if(plottype == "studentst") {
alpha <- 1.
v <- 3
variance = 7.5
sigma2 <- variance * (v-2) / v
mu <- 20

dgt <- function (x,v, sigma2,mu) gamma((v+1)/2)/(gamma(v/2)*sqrt(v*pi*sigma2)) *
  (1+1/v * ((x-mu)^2/sigma2))^(-(v+1)/2)


xlist <- seq(5,35, by = 0.01)
pgt <- dgt(xlist, v, sigma2, mu)
par(fig = c(0,1,0,1))
plot(xlist, pgt,
     type = "l",
     lwd = 3,
     col = "gray70",
     xlab = "x",
     ylab = "f(x)",
     las = 1,
     ylim = c(0, 0.25),
     yaxs = "i")

# compare to normal distribution with same variance

pn <- dnorm(xlist, mean = mu, sd = variance^0.5)

lines(xlist, pn,
      lwd = 3,
      col = "black")
# make inset figure
par(fig = c(0.05,0.50, 0.3, 0.975), new = T)
xlist.short <- xlist[which(xlist<=14)]
pgt.short <- pgt[which(xlist<=14)]
pn.short <- pn[which(xlist<=14)]
plot(xlist.short, pgt.short,
     type = "l",
     lwd = 3,
     col = "gray70",
     xlab = "",
     ylab = "",
     las =1,
     ylim = c(0, 0.005),
     yaxs = "i",
     axes = F)
box()
axis(1, at = c(6,10,14), cex.axis = 0.8, line = 0.0)
axis(2, at = c(0, 0.005), labels = F, cex.axis = 0.8, las =1)
lines(xlist.short, pn.short,
      lwd = 3,
      col = "black")
}
', sep = "\n")
}
