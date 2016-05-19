# Example inspired by
# https://idontgetoutmuch.wordpress.com/2014/09/09/fun-with-extended-kalman-filters-4/

# Logistic growth function
logistG <- function(r, p, k, t){
    k * p * exp(r*t) / (k + p * (exp(r*t) - 1))
}

k <- 100
p0 <- 0.1*k
r <- 0.2
deltaT <- 0.1
ORDER <- 2

# Let's create some sample data:
set.seed(12345)

obsVariance <- 25
nObs = 250
nu <- rnorm(nObs, mean=0, sd=sqrt(obsVariance))  # noise term
pop <- c(p0, logistG(r, p0, k, (1:(nObs-1))*deltaT)) + nu # Signal = logistic function + noise

Estimate <- data.frame(Rate=rep(NA, nObs),
                       Population=rep(NA,nObs))

library(numDeriv)
a <- function(x, k, deltaT){
    c(r=x[1],
      logistG(r=x[1], p=x[2], k, deltaT))
}
theta <- t(rep(0.1, ORDER))

# Evolution error
Q <- diag(c(0, 0))
# Observation error
R <-  obsVariance
# Prior
x <- as.matrix(rep(1, ORDER))
Sigma <-  diag(ORDER) * .1

for(i in ORDER:nObs){
    # Observation
    theta <- theta
    Sigma <- Sigma + 
    
    H <- as.matrix(pop[(i - ORDER + 1):i])
    z <- theta %*% H
    r <- H %*% theta
    
    
    # Filter 
    SigTermInv <- solve(theta %*% Sigma %*% t(theta) + R)
    
    xf <- x + Sigma %*% t(theta) %*%  SigTermInv %*% (z - theta %*% x)
    
    Sigma <- Sigma - Sigma %*% t(theta) %*% SigTermInv %*% theta %*% Sigma 
    
    A <- jacobian(a, x=x, k=k, deltaT=deltaT)   
    K <- A %*% Sigma %*% t(theta) %*% solve(theta %*% Sigma %*% t(theta) + R)
    Estimate[i,] <- x
    
    # Predict
    x <- a(x=xf, k=k, deltaT=deltaT) + K %*% (y - theta %*% xf)
    Sigma <- A %*% Sigma %*% t(A) - K %*% theta %*% Sigma %*% t(A) + Q
}

# Plot output
op <- par(mfrow=c(2,1))
time <- c(1:nObs)*deltaT
plot(y=pop, x=time, t='l', main="Population growth", 
     xlab="Time", ylab="Population")
curve(logistG(r, p0, k, x),  from=0, to=max(time), col=2, add=TRUE, lwd=1)
lines(y=Estimate$Population, x=time, col="orange", lwd=2)
legend("bottomright", 
       legend=c("Data","Actual", "Estimate"), 
       bty="n",
       col=c("black", "red", "orange"),
       lty=1, lwd=2)
plot(y=Estimate$Rate, x=time, t='l', main="Estimated growth rate", 
     xlab="Time", ylab="Rate", col="orange", lwd=2)
abline(h=r, col=adjustcolor("red", alpha=0.5), lwd=2)
legend("topright", 
       legend=c("Actual", "Estimate"), 
       bty="n",
       col=c("red", "orange"),
       lty=1, lwd=2)
par(op)