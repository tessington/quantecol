fig3.1 <- function(viewcode = F) {

  #' Structured population Demo
  #'
  #' Runs several examples of structured population analysis based on the 5-age-c.ass model illustrated in Chapter 3.  This includes the coding of structured models using both matrix and non-matrix notation, and use of eigenvalues and eigenvectors.  Returns the graphic in Figure 3.1.
  #' @param TRUE or FALSE (default) indicating whether to print the function code
  #' @return A list containing population growth rate and stable age distribution calculated via simulation and via eigenvalue / eigenvector analysis, elasticities, and a plot of transitent and stable model dynamics
  #' @export
  #'
  #' @examples
  #' # Run the code, and print output at console
  #' fig3.1()
  #'
  #' # Run the code, assign output to an object
  #' output <- fig3.1()
  #' print(output)
  #'
  #' # View the code
  #' fig3.1(viewcode = T)
  #'

## Define Parameters

F3 <- 10
F4 <- 15
F5 <- 20
S1 <- 0.2
S2 <- 0.3
S3 <- 0.4
S4 <- 0.5

# Initial Conditions
N1.start <- 20
N2.start <- 15
N3.start <- 10
N4.start <- 5
N5.start <- 1

tmax <- 50
years <- 0:tmax
n.loop <- length(years)

# Setup matrix to hold model output, with each year in row, each age in column

output <- matrix (NA, nrow = length(years), ncol = 5)
colnames(output) <- c("Age 1", "Age 2", "Age 3", "Age 4", "Age 5")

# Input starting values into the first row

output[1,]<- c(N1.start, N2.start, N3.start, N4.start, N5.start)

####################################################################
for (i in 2:n.loop){
  output[i,1]<- output[i-1, 3] * F3 + output[i, 4] * F4 + output[i,5] * F5
  output[i,2]<- output[i-1,1] * S1
  output[i,3]<- output[i-1,2] * S2
  output[i,4]<- output[i-1,3] * S3
  output[i,5]<- output[i -1,4] * S4
}

# calculate stable age distribution in year 50
ntotal <- rowSums(output)

age.distribution <- output[nrow(output),]  / ntotal[length(ntotal)]

# Calculate population growth rate in year 50
lambda_sim <- ntotal[51] / ntotal[50]

#### Code the model with matrix algebra
# Create Transition Matrix

A <- matrix(0, nrow = 5, ncol = 5)
A[1,3:5] <- c(F3, F4, F5)
A[2,1] <- S1
A[3,2] <- S2
A[4,3] <- S3
A[5,4] <- S4


for (i in 2:n.loop){
  output[i,] <- A %*% output[i-1,]
}

# calculate population growth rate using eigenvalue
ev <- eigen(A)
eigen.vals<- ev$values
eigen.vects <- ev$vectors

lambda <- Re(eigen.vals[1])
w <- Re(eigen.vects[,1])

age.distribution_eig <- w / sum(w)

# calculate elasticitiy
evl <- eigen(t(A))
v <- Re(evl$vectors[,1])
sp <- sum(v * w)
vi.wj <- v %o% w
e <-  A  / lambda * (vi.wj / sp)

# reset graphic parameters to default
reset_graphics_par()

# generate colors for the plot
cols <- viridis::viridis(n=5)
# plot only the first 30 years
matplot(years[1:30], output[1:30,1:5],
        type = "l",
        lty = "solid",
        lwd = 2,
        col = cols,
        xlab = "Year",
        ylab = "Abundance",
        las = 1)

par(xpd = NA)
legend(x = 21, y = 200, legend = c("Age-1", "Age-2", "Age-3", "Age-4", "Age-5"), lty = "solid",
       lwd = 2, col = cols, border = "black", cex = 0.8)
text(x = 2.5, y = 210, labels = "Transient")
text(x = 15, y = 300, labels = "Stable")
if(viewcode) cat('## Define Parameters

F3 <- 10
F4 <- 15
F5 <- 20
S1 <- 0.2
S2 <- 0.3
S3 <- 0.4
S4 <- 0.5

# Initial Conditions
N1.start <- 20
N2.start <- 15
N3.start <- 10
N4.start <- 5
N5.start <- 1

tmax <- 50
years <- 0:tmax
n.loop <- length(years)

# Setup matrix to hold model output, with each year in row, each age in column

output <- matrix (NA, nrow = length(years), ncol = 5)
colnames(output) <- c("Age 1", "Age 2", "Age 3", "Age 4", "Age 5")

# Place starting values into the first row

output[1,]<- c(N1.start, N2.start, N3.start, N4.start, N5.start)

# Iterate to run the model
for (i in 2:n.loop){
  output[i,1]<- output[i-1, 3] * F3 + output[i, 4] * F4 + output[i,5] * F5
  output[i,2]<- output[i-1,1] * S1
  output[i,3]<- output[i-1,2] * S2
  output[i,4]<- output[i-1,3] * S3
  output[i,5]<- output[i -1,4] * S4
}

# calculate stable age distribution in year 50
ntotal <- rowSums(output)

age.distribution <- output[nrow(output),]  / ntotal[length(ntotal)]

# Calculate population growth rate in year 50
lambda_sim <- ntotal[51] / ntotal[50]

#### Code the model with matrix algebra
# Create Transition Matrix

A <- matrix(0, nrow = 5, ncol = 5)
A[1,3:5] <- c(F3, F4, F5)
A[2,1] <- S1
A[3,2] <- S2
A[4,3] <- S3
A[5,4] <- S4


for (i in 2:n.loop){
  output[i,] <- A %*% output[i-1,]
}

# calculate population growth rate using eigenvalue
ev <- eigen(A)
eigen.vals<- ev$values
eigen.vects <- ev$vectors

lambda <- Re(eigen.vals[1])
w <- Re(eigen.vects[,1])

age.distribution_eig <- w / sum(w)

# calculate elasticitiy
evl <- eigen(t(A))
v <- Re(evl$vectors[,1])
sp <- sum(v * w)
vi.wj <- v %o% w
e <-  A  / lambda * (vi.wj / sp)


# generate colors for the plot
cols <- viridis::viridis(n=5)
# plot only the first 30 years
matplot(years[1:30], output[1:30,1:5],
        type = "l",
        lty = "solid",
        lwd = 2,
        col = cols,
        xlab = "Year",
        ylab = "Abundance",
        las = 1)

par(xpd = NA)
legend(x = 21, y = 200, legend = c("Age-1", "Age-2", "Age-3", "Age-4", "Age-5"), lty = "solid",
       lwd = 2, col = cols, border = "black", cex = 0.8)
text(x = 2.5, y = 210, labels = "Transient")
text(x = 15, y = 300, labels = "Stable")
', sep = "\n")
return(list(lambda_from_simulation = lambda_sim, lambda_from_eiegenvalue = lambda,  stable_age_sim = age.distribution, stable_age_eigen =  age.distribution_eig, elasticity = e))

}
