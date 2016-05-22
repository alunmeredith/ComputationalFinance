#### AUTOREGRESSION MODEL #########
# ar(timeSeries, aic = F, order.max = order) #alternate method
autoregression <- function(timeSeries, order) {
    
    # Construct a N order design matrix from time series
    N <- length(timeSeries)
    Design <- matrix(nrow = N - order + 1, ncol = order)
    for (i in seq_len(order)) {
        Design[,i] <- timeSeries[i:(N-order+i)]
    }
    # zoo::rollapply(timeSeries, order, print) # Alternative method for design matrix

    # Remove last row and first 'order' observations from targets / Design
    Design <- Design[-(N-order+1),]
    targets <- timeSeries[(order+1):N]
    
    # solve inverse problem to fit AR model with least square
    weights <- solve(t(Design) %*% Design) %*% t(Design) %*% targets
    #weights <- lsfit(Design, targets) # Other ways to fit
    model <- lm(targets ~ Design)
    
    # Make predictions
    predictions <- Design %*% weights
    
    # Calculate residual
    residuals <- targets - predictions
    
    # calculate variance
    variance = as.numeric(var(residuals))
    
    # Return: weights, residuals, predictions, variance of residuals
    return(list(Design = Design, targets = targets, weights = weights, predictions = predictions, residuals = residuals, variance = variance))
}


# MODEL
# data.frame(predictions, uncertainties, targets = timeSeries, errors)

#
prediction_update <- function(state, var_evo){
    uncertainty <- state$uncertainty
    weights <- state$weights
    # Update weights/uncertainty
    # Weights stays the same
    uncertainty <- uncertainty + var_evo
    return(list(weights = weights, uncertainty = uncertainty))
}

# Correct 
correction <- function(state, target, lagPoints, var_obs) {
    uncertainty <- state$uncertainty
    weights <- state$weights
    lagPoints <- matrix(lagPoints)
    
    residual <- target - t(weights) %*% lagPoints
    S <- t(lagPoints) %*% uncertainty %*% (lagPoints) + var_obs
    K <- uncertainty %*% lagPoints %*% MASS::ginv(S)
    # Update weights on new data
    weights <- weights + K %*% residual
    
    # Update uncertainty on new data
    uncertainty <- (diag(ORDER) - K %*% t(lagPoints)) %*% uncertainty
        
    return(list(weights = weights, uncertainty = uncertainty))
}

# Simulation
simulation <- function(dat_len, weights, noise_mean, noise_var) {
    # Should yield weights .7, .3
    order <- length(weights)
    
    targets <- vector()
    targets[1:order] <- 1
    
    for (i in (order + 1):dat_len) {
        target <- 0
        for (w in seq_along(weights)) {
            target <- target + targets[i - w] * weights[w]
        }
        targets[i] <- target + (rnorm(noise_mean, noise_var) - noise_mean/2)
    }
    
    return(targets)
}

rsquared <- function(predictions, targets) {
    yhat <- mean(targets, na.rm = T) 
    SSE <- sum((predictions - yhat)^2, na.rm = T)
    SST <- sum((targets - yhat)^2, na.rm = T)
    return(SSE/SST)
}