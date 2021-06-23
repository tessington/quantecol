runge.kutta <- function(Xstart, modelpars, fun, deltat, n.years) {

  #Create a function to apply the RK for one time step
  run.kutta.fun <- function(X, modelpars, fun = fun) {
    # this code takes all of the elements in the list modelpars, and creates objects for each value of the list that can be used
    # in the function.  For instance, if the list is modelpars = list(a =  0.1, b = 0.2), it will create
    # an object "a" and assign the value 0.1, and an object "b" and addign the value 0.2.
    list2env(modelpars, envir = environment())
    fn1 <- function(modelpars, X) fun(modelpars, X)
    K1 <- fun(modelpars, X)
    K2 <- fun(modelpars, X+0.5 * deltat * K1)
    K3 <- fun(modelpars, X + 0.5 * deltat * K2)
    K4 <- fun(modelpars, X+deltat*K3)
    Xnew <-X+deltat * (K1/6 + K2 / 3 + K3 / 3 + K4/6)
    return(Xnew)
  }
  # setup output vector
  n.vars <- length(Xstart)
  timesteps <- seq(0, n.years, by = deltat)
  n.steps <- length(timesteps)
  output <- matrix(NA, nrow = n.steps, ncol = n.vars +1)
  # assign starting values
  output[1,]<-c(0, Xstart)
  # Run through time steps applying RK for each timestep
  for (i in 2:n.steps) {
    Xnew <- run.kutta.fun(output[i-1,2:3],modelpars, fun)
    output[i,] <- c(timesteps[i], Xnew)
  }
  return(output)
}
