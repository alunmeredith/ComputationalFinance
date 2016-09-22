#### Set up data ############################################################
#############################################################################

Residuals <- readRDS("Residuals.rds")
predictors <- read.csv("Predictors.csv")
source('function_library.R')
source('kalman_2.R')

targets <- Residuals$targets
dates <- predictors$observation_date

# Join predictors with the same observation dates together
Predictors <- predictors[, c("OILPRICE", "PMI", "INCOME", "UNEMPLOYMENT", "POPULATION")]
rownames(Predictors) <- dates

# Interpolate corp profit as constant in between changes. 
Predictors$CORP.PROFIT <- NA
Predictors$CORP.PROFIT[na.omit(match(na.omit(predictors$observation_date.3), predictors$observation_date))] <- na.omit(predictors$CORP.PROFIT)
for (i in seq_along(Predictors$CORP.PROFIT)) {
    if(is.na(Predictors$CORP.PROFIT[i])) {
        Predictors$CORP.PROFIT[i] <- (Predictors$CORP.PROFIT[i - 1])
    }
}

# Join S&P PE ratio
PE <- read.csv("MULTPL-SP500_PE_RATIO_MONTH.csv")
Predictors$SP_PE <- rev(PE$Value)
Predictors <- as.matrix(Predictors)
# Remove leftover data frames
rm(PE)
rm(predictors)
rm(i)

# Standardise (mean = 0, s.d. = 1)
Predictors_std <- apply(Predictors, 2, scale)
rownames(Predictors_std) <- dates

# Kalman filter
ORDER <- 3
Predictors_AR <- autoregression(Predictors[, "OILPRICE"], ORDER)
Predictors_kal <- apply(Predictors, 2, function(x) {
    kalman(x, ORDER, V =  var(Predictors_AR$residuals), W = diag(ORDER) * 0.001, theta = rep(1/ORDER, ORDER), P = diag(ORDER) * 100)$Estimate
})

# Kalman and standardised
Predictors_kal_std <- apply(Predictors_std, 2, function(x) {
    kalman(x, ORDER, V =  var(Predictors_AR$residuals), W = diag(ORDER) * 0.001, theta = rep(1/ORDER, ORDER), P = diag(ORDER) * 100)$Estimate
})

################################################################################################
####### LASSO #################################################################################

# Lasso over all data
library(glmnet)

results <- data.frame(rsquared = numeric(),
                      pvalue = numeric(), 
                      OILPRICE = numeric,
                      PMI = numeric(),
                      INCOME = numeric(),
                      UNEMPLOYMENT = numeric(),
                      POPULATION = numeric(), 
                      CORP.PROFIT = numeric(),
                      SP_PE = numeric())


models <- c("Kalman1", "Kalman5", "Kalman10", "AR3")
predictors_set <- list(Predictors, Predictors_std, Predictors_kal, Predictors_kal_std)
for (predictor in predictors_set) {
    
    for (model in models) {
        set.seed(44)
        targets <- Residuals[11:nrow(Residuals), model]
        cvfit = cv.glmnet(predictor[11:241,], targets, intercept = F, lambda = c(0,2^(-10:20)), nfolds = 5)
        pred <- predict(cvfit, newx = predictor, s = "lambda.min")
        
        r2 <- rsquared(pred, targets = targets)
        p <- t.test(pred, targets)$p.value
        c <- coef(cvfit, s = "lambda.min")
        coefs <- rep(0, 7)
        coefs[c@i] <- c@x
        result <- c(rsquared = r2, pvvalue = p, coefs)
        names <- colnames(results)
        
        results <- rbind(results, result)
        colnames(results) <- names
        
    }

}
results$model <- models
results$predictors <- rep(c("Raw", "Standardised", "Kalman", "Kalman.Standardised"), each = length(models))
#########################################
#### SLIDING WINDOW  #####################################

index <- 11:190
predictions <- data.frame(row.names = index)

for (model in c("Kalman1", "Kalman5", "Kalman10", "AR3")) {
    selected <- data.frame(row.names = rownames(coef(cvfit, s = "lambda.min"))[-1])
    pred = vector()
    for (i in index) {
        cvfit = cv.glmnet(Predictors_std[i:(49+i),], Residuals[i:(49+i),model], 
                          intercept=FALSE, lambda = c(0,2^(-10:20)), nfolds = 5)
        pred = c(pred, predict(cvfit, newx = Predictors_std[(50+i):(51+i),], s = "lambda.min")[1])
        
        coefs <- coef(cvfit, s = "lambda.min")
        selected[, paste0("x",i)] <- 0
        selected[coefs@i, paste0("x",i)] <- coefs@x
    }
    
    # Selected Coeficients
    selected <- data.frame(t(selected))
    variable.counts <- data.frame(count = apply(selected, 2, function(x) sum(x > 0)))
    variable.counts$feature <- rownames(variable.counts)
    
    # Predictions
    predictions[,model] <- pred
    
    t <- t.test(predictions[,model], Residuals[index+50,model])$p.value
    print(paste("pvalue:", t))
    
    r <- rsquared(predictions[,model], Residuals[index+50,model])
    print(paste("rsquared:", r))
    
    #Plots
    gg2 <- ggplot(variable.counts, aes(y = count, x = feature, fill = feature)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        theme_few() +
        scale_fill_few("dark") +
        labs(title = model)

    png(paste0("count_", model,".png"), width = 540, height = 240)
    gg2
    invisible(dev.off())
    gg2

    colors <- few_pal("dark")(7)
    active_times <- apply(selected, 2, function(x) which(x > 0))
    gg1 <- ggplot(mapping = aes(x = index)) +
        geom_point(aes(y = (index %in% (active_times$OILPRICE + index[1] + 50)) * 1), col = colors[1]) +
        geom_point(aes(y = (index %in% (active_times$PMI + index[1] + 50)) * 2), col = colors[2]) +
        geom_point(aes(y = (index %in% (active_times$INCOME + index[1] + 50)) * 3), col = colors[3]) +
        geom_point(aes(y = (index %in% (active_times$UNEMPLOYMENT + index[1] + 50)) * 4), col = colors[4]) +
        geom_point(aes(y = (index %in% (active_times$POPULATION + index[1] + 50)) * 5), col = colors[5]) +
        geom_point(aes(y = (index %in% (active_times$CORP.PROFIT + index[1] + 50)) * 6), col = colors[6]) +
        geom_point(aes(y = (index %in% (active_times$SP_PE + index[1] + 50)) * 7), col = colors[7]) +
        scale_y_continuous(limits = c(.5,7.5), breaks = 1:7, labels = names(selected)) +
        theme_few() +
        scale_x_continuous(limits = c(50, 241)) +
        labs(x = "Month", y = "Feature", title = model)

    png(paste0("TimeSeries", model, ".png"), width = 720, height = 240)
    gg1
    invisible(dev.off())
    gg1
}

# Plot residuals over time ####
R <- Residuals
R$prediction <- c(rep(NA, 53), predictions$Kalman10, rep(NA, 8))
R <- R %>% select(-ARIMA011, -ARIMA022, -targets) 
R <- tidyr::gather(R, model, Residual)
R$Month <- 1:241

gg3 <- ggplot(data = R, aes(x = Month, color = model)) +
    geom_point(aes(y = Residual, size = model)) +
    scale_y_continuous(limits = c(-200, 200)) +
    scale_color_manual(values = c(colors[1:5])) +
    theme_few() +
    geom_hline(yintercept = 0) + 
    scale_size_manual(values = c(1.2,1.2,1.2,1.2,2))

png("Residuals.png")
gg3
invisible(dev.off())
gg3
