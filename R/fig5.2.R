fig5.2 <- function(viewcode  = FALSE) {
#' Demographic stochasticity
#'
#' Generates figure 5.2, by simulating the time dynamics of a population demographic stochasticity (where the number of births and deaths each year is random because the population size is small) is important. Each time the simulation is run, a different outcome will be seen.
#' @param viewcode TRUE or FALSE (default) indicating whether to print the function code
#' @export
#' @examples
#' # View the code
#' fig5.2(viewcode = TRUE)
#' # See a randomized version of Figure 5.2
#' fig5.2()


n.start <- 10
b <- 0.3
d <- 0.25

n.years <- 20
n.t <- rep(NA, n.years)
n.t[1]<- n.start

for (i in 2:n.years){
  n.t[i]<- n.t[i-1]+rbinom(n=1, size = n.t[i-1], prob = b)-rbinom(n=1, size = n.t[i-1], prob = d)
}

reset_graphics_par()
ylims <- c(0, 50)
plot(1:n.years,
     n.t,
     type = "l",
     lwd =3,
     col = "black",
     xlab = "Years",
     ylab = "Population Size",
     xaxs = "i",
     yaxs = "i",
     ylim = ylims,
     las = 1,
     cex.lab = 1.5
)
points(1:n.years,
       n.t,
       pch = 21,
       bg = "black")
if(viewcode) cat(readLines(con = "txt/fig5.2.txt"), sep = "\n")

}
