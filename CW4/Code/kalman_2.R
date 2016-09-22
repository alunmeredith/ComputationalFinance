kalman <- function(targets, ORDER, V, W, theta, P) {

    Dat <- data.frame(row.names = 1:length(targets))
    Estimate_theta <- matrix(NA, nrow = length(targets), ncol = ORDER)
    Estimate_P <- matrix(NA, nrow = length(targets), ncol = length(P))
    for (i in (ORDER + 1):length(targets)) {
        # Prediction
        new_theta <- theta
        new_P <- P + W
        
        # Observation
        H <- t(targets[(i - ORDER):(i - 1)])
        r <- targets[i] - H %*% new_theta
        
        S <- H %*% new_P %*% t(H) + V
        K <- P %*% t(H) %*% (1/S)
        
        # Correction
        theta <- new_theta + K %*% r
        P <- (diag(ORDER) - K %*% H) %*% new_P

        # Save 
        Estimate_theta[i,] <- as.vector(theta)
        Estimate_P[i,] <- as.vector(P)
        Dat[i, "Estimate"] <- H %*% new_theta
    }
    colnames(Estimate_theta) <- paste0("Theta", 1:ncol(Estimate_theta))
    colnames(Estimate_P) <- paste0("P", 1:ncol(Estimate_P))
    Dat <- cbind(targets, Dat, Estimate_theta, Estimate_P)
    return(Dat)
}

