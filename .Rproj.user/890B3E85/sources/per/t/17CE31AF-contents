
rm(list = ls())

rx=0.5
Kx=100
alpha=0.95
ry=0.5
Ky=100
beta=.95

mult=c(0.95,1)
deltat=0.1
t.perturb=20
t.run<-100



xstar <- 51
Ystar <- 49

alpha=0.75
beta=0.75

Ky <- Mstar + Bstar*alpha
Kx <- Bstar + Mstar*beta

# Use the Adams - Bashford method to simulate the differential equations.


run.dxtdt<-function(Xstart,
                    Ystart,
                    parslist,
                    t.perturb = 20,
                    perturb = c(1.05,1),
                    years <- 100) {
  rm <- parslist$rm
  Km<-parslist$Km
  alpha<-parslist$alpha
  rb <- parslist$rb
  Kb <- parslist$Kb
  beta<-parslist$beta,


  n.steps<-round(years/deltat,0)

  dxdt<-function(X,Y,rx,Ky,alpha) rx*X*(1-(X+alpha*Y)/Kx)
  dydt<-function(X,B,rb,Kb,beta) ry*Y*(1-(Y+beta*X)/Ky)

  output<-matrix(NA,nrow=n.steps,ncol=2)

  output[1,1:2]<-c(Xstart, Ystart)
  dXYdt.t<-matrix(NA,nrow=n.steps,ncol=2)
  dXYdt.t[1,1]<-dxdt(Xstart,Ystart,rx,Kx,alpha)
  dXdt.t[1,2]<-dydt(Xstart,Ystart,ry,Ky,beta)
  # use the Euler method for first time step
  output[2,1]<-output[1,1]+deltat*dXYdt.t[1,1]
  output[2,2]<-output[1,2]+deltat*dXYdt.t[1,2]

  # now use adams bashford
  X.vec[2,2]<-outputc[1,2]+deltat*dbdt(Mstart,Bstart,rb,Kb,beta)

  for (t in 3:n.steps){
    dXdt.save[t-1,]<-c(dmdt(X.vec[t-1,1],X.vec[t-1,2],rm,Km,alpha),dbdt(X.vec[t-1,1],X.vec[t-1,2],rb,Kb,beta))
    X.vec[t,1]<-X.vec[t-1,1]+1/2*deltat*(3*dXdt.save[t-1,1]-dXdt.save[t-2,1])
    X.vec[t,2]<-X.vec[t-1,2]+1/2*deltat*(3*dXdt.save[t-1,2]-dXdt.save[t-2,2])
    if (t*deltat==t.perturb) X.vec[t,]<-X.vec[t,]*mult

  }

  return(X.vec)

}


Mstart=Mstar
Bstart=Bstar

output<-run.dxdt(Mstar,Bstar,rm, Km, alpha, rb, Kb, beta,t.perturb,mult=c(1.05,1),t.run.delta.t)

#plotfilename <- "stability_example.pdf"
#pdf(file = plotfilename, width = 5, height = 2.5)
#par(las =1,mfrow = c(1,2), mar = c(2,2,1,1))

plot((1:nrow(output))*deltat,output[,1],type="l",col="black",lwd=3,xlab="",ylab="",ylim=c(0,110),xaxs="i", axes = "F")
box()
mtext(side = 1, line = 1, text = "Time")
mtext(side = 2, line = 1, text = "State Variable", las = 0)
lines(1:nrow(output)*deltat,
      output[,2],
      type = "l",
      lwd = 3,
      col = "gray50")

alpha=1.25
beta=1.25

Km <- Mstar + Bstar*alpha
Kb <- Bstar + Mstar*beta
Mstart=Mstar
Bstart=Bstar

output<-run.dxdt(Mstar,Bstar,rm, Km, alpha, rb, Kb, beta,t.perturb,mult=c(1.05,1),t.run.delta.t)


par(las =1)
plot((1:nrow(output))*deltat,output[,1],type="l",col="black",lwd=3,xlab="",ylab="",ylim=c(0,110),xaxs="i", axes = F)
box()
mtext(side = 1, line = 1, text = "Time")
mtext(side = 2, line = 1, text = "State Variable", las = 0)

lines(1:nrow(output)*deltat,
      output[,2],
      type = "l",
      lwd = 3,
      col = "gray50")
#dev.off()
#system2("open",args = c("-a Skim.app",plotfilename))

# moviefun<-function(output){
#   thin=5
#   output<-output[seq(1,nrow(output),by=thin),1:2]
#   n.output<-nrow(output)
#   par(las=1)
#   for (i in 1:n.output) {
#   plot((1:i)*deltat*thin,output[1:i,1],type="l",col="blue",lwd=3,xlab="time",ylab="state variable",ylim=c(0,100),xaxs="i",xlim=c(0,100))
#   par(new=T)
#   plot((1:i)*deltat*thin,output[1:i,2],type="l",col="red",lwd=3,xlab="time",ylab="state variable",ylim=c(0,100),xaxs="i",xlim=c(0,100))
#   par(new=T)
#   plot((i)*deltat*thin,output[i,1],type="p",pch=21,col="blue",bg="blue",lwd=2,ylim=c(0,100),xaxs="i",xlim=c(0,100),xlab="",ylab="",axes=F)
#   par(new=T)
#   plot((i)*deltat*thin,output[i,2],type="p",pch=21,col="red",bg="red",lwd=2,ylim=c(0,100),xaxs="i",xlim=c(0,100),xlab="",ylab="",axes=F)
#   par(new=F)
# }
# }
#
#
# # function to just plot the barnacles, but show the perturbation size on the top panel
# perturb<-function(output,Mstar,perturb.scale=c(-10,10)){
#   dev.hold()
#   thin=10
#   output<-output[seq(1,nrow(output),by=thin),1:2]
#   n.output<-nrow(output)
#   par(las=1)
#   layout(c(2,1),heights=c(4,9))
#   M<-output[,1]
#   m<-M-rep(Mstar,n.output)
#   par(mai=c(1.5,1.5,0,.5))
#   for (i in 1:n.output) {
#     plot((1:i)*deltat*thin,M[1:i],type="l",col="blue",lwd=3,xlab="time",ylab="state variable",ylim=c(0,1),xaxs="i",xlim=c(0,100))
#     points((i)*deltat*thin,M[i],type="p",pch=21,col="blue",bg="blue")
#     lines(rep((i)*deltat*thin,2),c(Mstar,M[i]),lwd=2,col="red")
#     abline(h=Mstar,lty="dotted",col="blue")
#     par(mai=c(0.0,1.5,0,.5),new=F)
#     plot((1:i)*deltat*thin,m[1:i],type="l",lwd=2,xaxs="i",col="red",xlim=c(0,100),ylim=perturb.scale,xlab="",ylab="",axes=F)
#     points((i)*deltat*thin,m[i],type="p",pch=21,col="red",bg="red")
#     abline(h=0)
#     ani.pause()
#   }
# }
#
#
#
# perturb(output,Mstar,c(-75,75))
#
# moviefun(output)
# library(animation)
# saveGIF(moviefun(output),movie.name="unstable.gif")
#
# saveGIF(perturb(output,Mstar,c(-1,75)),movie.name="showunstable.gif")
#
# a=  5
# b=	2
# c=	0.3
# d=	0.3
# h=	0.05
# K=	7
#
# Nstar=d/(b*((c-d*h)))
# Pstar=1/b*(a+a*h*b*Nstar-a*Nstar/K-a*h*b*Nstar^2/K)
# run.dxdt.pp<-function(Nstart,Pstart, a, b, c, d, h, K,t.perturb,mult,t.run,delta.t) {
#   n.steps<-round(t.run/deltat,0)
#
#   dNdt<-function(N,P,a,b,c,d, h, K) a*N*(1-(N)/K)-b*N*P/(1+b*N*h)
#   dPdt<-function(N,P,a,b,c,d, h, K) c*b*N*P/(1+b*N*h)-d*P
#
#   X.vec<-matrix(NA,nrow=n.steps,ncol=2)
#
#   X.vec[1,1:2]<-c(Nstart, Pstart)
#   dXdt.save<-matrix(NA,nrow=n.steps,ncol=2)
#   dXdt.save[1,1]<- dNdt(Nstart,Pstart,a,b,c,d,h,K)
#   dXdt.save[1,2]<-dPdt(Nstart,Pstart,a,b,c,d,h,K)
#   X.vec[2,1]<-X.vec[1,1]+deltat*dNdt(Nstart,Pstart,a,b,c,d,h,K)
#   X.vec[2,2]<-X.vec[1,2]+deltat*dPdt(Nstart,Pstart,a,b,c,d,h,K)
#
#   for (t in 3:n.steps){
#     dXdt.save[t-1,]<-c(dNdt(X.vec[t-1,1],X.vec[t-1,2],a,b,c,d,h,K),dPdt(X.vec[t-1,1],X.vec[t-1,2],a,b,c,d,h,K))
#     X.vec[t,1]<-X.vec[t-1,1]+1/2*deltat*(3*dXdt.save[t-1,1]-dXdt.save[t-2,1])
#     X.vec[t,2]<-X.vec[t-1,2]+1/2*deltat*(3*dXdt.save[t-1,2]-dXdt.save[t-2,2])
#     if (t*deltat==t.perturb) X.vec[t,]<-X.vec[t,]*mult
#
#   }
#
#   return(X.vec)
#
# }
# mult=c(0.95,1)
# delta.t=0.1
# t.perturb=20
# t.run<-100
#
# output<-run.dxdt.pp(Nstar,Pstar,a,b,c,d,h,K,t.perturb,mult=c(1.05,1),t.run,delta.t)
#
# moviefun.pp<-function(output){
#   dev.hold()
#   thin=5
#   output<-output[seq(1,nrow(output),by=thin),1:2]
#   n.output<-nrow(output)
#   par(las=1)
#   for (i in 1:n.output) {
#     plot((1:i)*deltat*thin,output[1:i,1],type="l",col="blue",lwd=3,xlab="time",ylab="state variable",ylim=c(0,1),xaxs="i",xlim=c(0,100))
#     par(new=T)
#     plot((1:i)*deltat*thin,output[1:i,2],type="l",col="red",lwd=3,xlab="time",ylab="state variable",ylim=c(0,1),xaxs="i",xlim=c(0,100))
#     par(new=T)
#     plot((i)*deltat*thin,output[i,1],type="p",pch=21,col="blue",bg="blue",lwd=2,ylim=c(0,100),xaxs="i",xlim=c(0,1),xlab="",ylab="",axes=F)
#     par(new=T)
#     plot((i)*deltat*thin,output[i,2],type="p",pch=21,col="red",bg="red",lwd=2,ylim=c(0,100),xaxs="i",xlim=c(0,1),xlab="",ylab="",axes=F)
#     par(new=F)
#     ani.pause()
#   }
# }
#
# perturb(output,Nstar,c(-.3,.3))
# moviefun.pp(output)
