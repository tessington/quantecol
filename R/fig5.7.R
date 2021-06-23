fig5.7 <- function(viewcode = FALSE) {
  #' Stochastic Density Dependent Model
  #'
  #' Generate Figure 5.7 that illustrates 25 random model runs of the Beverton-Holt density dependent model, where the parameters alpha and d (death rate) vary annually following a normal distribution.
  #' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
  #' @export
  #' @examples
  #' # Recreate a version of figure 5.7
  #' fig5.7()
  #' # View the code
  #' fig5.7(viewcode = TRUE)



alpha_bar <- 0.8
alpha_sigma <- 0.08
d_bar <- 0.55
d_sigma <- 0.055
Nstar <- 100
beta <- (alpha_bar - d_bar) / (Nstar * d_bar)
nyears <- 40

col <- viridis::plasma(25)
reset_graphics_par()

plot(c(), c(),
     type = "n",
     xlab = "Years",
     ylab = "Population Size",
     xaxs = "i",
     yaxs = "i",
     las =1,
     xlim = c(0, nyears),
     ylim = c(30, 160),
     xaxs = "i",
     yaxs = "i",
     las = 1
)



for (j in 1:25) {
  alpha_t <- rnorm(nyears, alpha_bar, alpha_sigma)
  d_t <- rnorm(nyears, d_bar, d_sigma)
  Nt <- rep(NA, nyears)
  Nt[1] <- Nstar*0.5

  for (i in 2:nyears) Nt[i] <-  Nt[i-1]*(alpha_t[i-1] / (1 + beta*Nt[i-1]) + (1-d_t[i-1]))


  lines(1:nyears, Nt,
        lwd = 2,
        col = col[j]
  )
}

# plot deterministic trajectory based on alpha_bar and d_bar
for (i in 2:nyears) Nt[i] <-  Nt[i-1]*(alpha_bar / (1 + beta*Nt[i-1]) + (1-d_bar))
lines(1:nyears, Nt,
      lwd = 3,
      col = "black")
if(viewcode) cat('# set up model parameters
# mean and standard deviation of alpha and d (death rate)
alpha_bar <- 0.8
alpha_sigma <- 0.08
d_bar <- 0.55
d_sigma <- 0.055
# Set equilibrium (deterministic) population size, and use this to set beta
Nstar <- 100
beta <- (alpha_bar - d_bar) / (Nstar * d_bar)
nyears <- 40

# prepare plotting
col <- viridis::plasma(25)

# create empty plot
plot(c(), c(),
     type = "n",
     xlab = "Years",
     ylab = "Population Size",
     xaxs = "i",
     yaxs = "i",
     las =1,
     xlim = c(0, nyears),
     ylim = c(30, 160),
     xaxs = "i",
     yaxs = "i",
     las = 1
)

# loop 25 times, each time drawing parameters, running stochastic model, and plot the result

for (j in 1:25) {
  alpha_t <- rnorm(nyears, alpha_bar, alpha_sigma)
  d_t <- rnorm(nyears, d_bar, d_sigma)
  Nt <- rep(NA, nyears)
  Nt[1] <- Nstar*0.5

  for (i in 2:nyears) Nt[i] <-  Nt[i-1]*(alpha_t[i-1] / (1 + beta*Nt[i-1]) + (1-d_t[i-1]))

  # add to plot
  lines(1:nyears, Nt,
        lwd = 2,
        col = col[j]
  )
}

# plot deterministic trajectory based on alpha_bar and d_bar
for (i in 2:nyears) Nt[i] <-  Nt[i-1]*(alpha_bar / (1 + beta*Nt[i-1]) + (1-d_bar))
lines(1:nyears, Nt,
      lwd = 3,
      col = "black")
', sep = "\n")
}
