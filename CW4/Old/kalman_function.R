kalman <- function(targets, dates, ORDER, Design, VAR_EVO, var_obs, PRIOR_sTATE) {
    # Define data frame to capture state at each point
    id = (1 + ORDER) : length(targets)
    dat <- data.frame(id = id,
                      date = dates[id],
                      targets = targets[id], 
                      weights_pred = I(vector(mode = "list", length = length(id))),
                      uncertainty_pred = I(vector(mode = "list", length = length(id))),
                      weights_corrected = I(vector(mode = "list", length = length(id))),
                      uncertainty_corrected = I(vector(mode = "list", length = length(id))),
                      prediction = NA,
                      residual = NA,
                      Design = I(vector(mode = "list", length = length(id)))
    )
    
    state <- PRIOR_STATE
    for (i in 1:length(dat$targets)) {
        # Weights Prediction 
        state <- prediction_update(state, VAR_EVO)
        dat[[i, "weights_pred"]] <- state$weights
        dat[[i, "uncertainty_pred"]] <- state$uncertainty
        
        # Correction
        state <- correction(state, targets[i], Design[i,], var_obs)
        dat[[i, "weights_corrected"]] <- state$weights
        dat[[i, "uncertainty_corrected"]] <- state$uncertainty
        
        # Prediction
        dat$prediction[i] <- as.numeric(t(state$weights) %*% Design[i,])
        dat$residual[i] <- dat$prediction[i] - dat$targets[i]
        dat[[i, "Design"]] <- Design[i,]
    }
    
    library(ggplot2)
    gg <- ggplot(data = dat, aes(x = id)) +
        geom_line(aes(y = targets)) +
        geom_line(aes(y = prediction), colour = "blue")
    gg
    
    return(list(data = dat, plot = gg))
}