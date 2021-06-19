fig5.8 <- function(viewcode = FALSE) {
  #' Stochastic Allee Effect Models
  #'
  #' Compare scenarios shown in Figure 5.8 where (1) stochasticity caused a population to drop below the Allee threshold and therefore collapse and (2) where stochasticity saved a population by allowing it to increase above the extinction threshold.
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #' @export
  #' @examples
  #' Re-create a version of Figure 5.8
  #' fig5.8()
  #' View code
  #' fig5.8(viewcode = TRUE)

# Inititialize population parameters
vsd<-0.04 # standard deviation of random variables
rbar<-0.5 # mean value of r
Nstart<-275 # initial populations size
Qextinct<-20 # quasi-extinction threshold
K<-1000 # carrying capacity
A<-250 # allee threshold

# Inititialize simulation parameters

nyears<-500
niters = 1

# make plot of delta N delta t vs. t
Nlist=seq(1,K,length=40)
deltaNout=matrix(0,40,2)
for (i in 1:40){
  N<-Nlist[i]
  deltaN<-rbar*N*(1-N/K)*(N/K-A/K)
  deltaNout[i,1:2]<-c(N,deltaN)
}



# loop through iterations
output<-matrix(0,1,nyears)
deltaNlist<-matrix(0,1,nyears)
output[1]<-Nstart
# run population for specified number of years
set.seed(87)
for (i in 2:nyears){
  Nold<-output[i-1]
  deltaN<-Nold*(rbar*(1-Nold/K)*(Nold/K-A/K) + rnorm(1,0,vsd))
  deltaNlist[i]<-deltaN
  output[i]<-max(Nold+deltaN,0)
}

reset_graphics_par
par = par(mfrow = c(1,2))
# plot output
plot(output[2:50],deltaNlist[1:50-1],
     type="l",col="black",
     ylab=expression(paste(Delta, "N")),
     xlab="Population Size",
     xlim=c(0,1200),
     ylim = c(-20, 60),
     las = 1)
abline(h=0,lty = "dashed")
# plot the actual production function
lines(deltaNout[,1],deltaNout[,2],type="l",lwd=2,col="black")

# repeat, starting below Allee threshold
Nstart<-225 # initial populations size
# loop through iterations
output<-matrix(0,1,nyears)
deltaNlist<-matrix(0,1,nyears)
output[1]<-Nstart
# run population for specified number of years
set.seed(128)
for (i in 2:nyears){
  Nold<-output[i-1]
  deltaN<-Nold*(rbar*(1-Nold/K)*(Nold/K-A/K) + rnorm(1,0,vsd))
  deltaNlist[i]<-deltaN
  output[i]<-max(Nold+deltaN,0)
}


# plot output
plot(output[2:50],deltaNlist[1:50-1],
     type="l",col="black",
     ylab=expression(paste(Delta, "N")),
     xlab="Population Size",
     xlim=c(0,1200),
     ylim = c(-20, 60),
     las = 1)
abline(h=0,lty = "dashed")
# plot the actual production function
lines(deltaNout[,1],deltaNout[,2],type="l",lwd=2,col="black")

if(viewcode) cat(readLines(con = "txt/fig5.8.txt"), sep = "\n")
}


