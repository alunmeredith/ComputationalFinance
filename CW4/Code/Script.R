# Libraries
library(readr)
source('function_library.R')
source('kalman_2.R')

### TEST DATA

# Guess parameters
ORDER <- 2
V <- autoregression(targets, ORDER)$variance
W = diag(ORDER) * 1e-7
# Initial state estimate
theta = rep(1/ORDER, ORDER)
P = diag(ORDER) * .1

# Run kalman filtering many times and cbind
ntimes <- 10
results <- data.frame()
for (i in 1:ntimes) {
    # Generate test data
    targets <- simulation(300, c(.3,.7), 1, .1)
    kal <- kalman(targets, ORDER, V, W, theta, P)
    kal$simulation.id <- i
    kal$index <- 1:nrow(kal)
    results <- rbind(results, kal)
}
results <- na.omit(results)
results$simulation.id <- as.factor(results$simulation.id)

### Run simulation n times with different orders of W
ntimes <- 5
targets <- simulation(30, c(.3,.7), 1, .1)
results2 <- data.frame()
for (i in 1:ntimes) {
    # Generate test data
    W = diag(ORDER) * 1 * 10 ^ ((i - 1 - round((ntimes-1)/2)) * 2)
    #W = matrix(1, nrow = ORDER, ncol = ORDER) * 1 * 10 ^ ((i - 1 - round((ntimes-1)/2)) * 2)
    kal <- kalman(targets, ORDER, V, W, theta, P)
    kal$W <- W[1,1]
    kal$index <- 1:nrow(kal)
    results2 <- rbind(results2, kal)
}
results2 <- na.omit(results2)
results2$W <- as.factor(results2$W)

#### Make plots of simulation data ################
###################################################

colors <- RColorBrewer::brewer.pal(4, name = "Set1")

# Plot kalman filter approximation of weights
#png("Test_Weight_Estimation.png")
gg <- ggplot(data = results, aes(x = index, group = simulation.id))
gg +
    geom_line(aes(y = Theta1, size = simulation.id, alpha = simulation.id), col = colors[1]) +
    geom_line(aes(y = Theta2, size = simulation.id, alpha = simulation.id), col = colors[2]) +
    theme_few() +
    geom_hline(yintercept = .3, col = colors[2], linetype = 2, size = 1.5) +
    geom_hline(yintercept = .7, col = colors[1], linetype = 2, size = 1.5) +
    scale_y_continuous(breaks = seq(.1, 1, .1), limits = c(0,1)) +
    scale_size_manual(values = c(1.5, rep(1, ntimes - 1))) +
    scale_alpha_manual(values = c(1, rep(.2, ntimes - 1))) +
    ylab("Theta") +
    xlab("Day")
dev.off()

# Plot kalman filter smoothing of data
png("Test_Estimation.png")
simulation_1 <- results[1:50,]
ggplot(data = simulation_1, aes(x = index)) + 
    geom_line(aes(y = Estimate)) + 
    geom_point(aes(y = targets), col = "blue") + 
    ylab("y") +
    xlab("Day") +
    theme_few()
dev.off()

# variation in W order of magnitude
# the shape of W also has an impact (but use diagonal matrix here)
# Lower orders of magnitude are "smoother", on a static function like this one more smoothing improves filtering but takes longer to "lock on" to the function. For the real data where the underlying function is not static this can lead to "chasing the dragon" where the underlying function changes faster than it is locked onto. 
png("W_variation.png")
ggplot(data = results2, aes(x = index, colour = W)) + 
    geom_point(aes(y = targets), col = "blue") + 
    geom_line(aes(y = Estimate)) +
    ylab("y") +
    xlab("Day") +
    theme_few() +
    scale_color_brewer(palette = "Set1")
dev.off()

### REAL DATA ######

# Read data
dat_raw <- read_csv("S&P500.csv")
targets <- (dat_raw$Close)

# Guess parameters
ORDER <- 3
AR_model <- autoregression(targets, ORDER)
V <- AR_model$variance
W = diag(ORDER) * 1e40

# Initial state estimate
theta = rep(1/ORDER, ORDER)
P = diag(ORDER) * 1e-3

# Kalman
kal <- kalman(targets, ORDER, V, W, theta, P)
kal$index <- 1:nrow(kal)

png("Test_smooth_3.png")
ggplot(data = kal, aes(x = index)) + 
    geom_line(aes(y = Estimate)) + 
    geom_point(aes(y = targets), col = "blue") + 
    ylab("y") +
    xlab("Day") +
    theme_few()
dev.off()

# Plot residual distribution
png("Residual_distribution.png")
qplot(kal$targets - kal$Estimate, alpha = .5) + 
    geom_histogram(aes(x = AR_model$residuals), fill = "blue", alpha = .5) +
    theme_few() +
    xlab("Residuals")
dev.off()

print( paste("AR RMSE: ", sqrt( sum(AR_model$residuals^2)) / length(AR_model$residuals)))
print( paste("Kalman RMSE: ", sqrt( sum((kal$targets - kal$Estimate)^2, na.rm = T)) / length(kal$targets) ))


### SUMMARY OF ERRORS FOR AUTOREGRESSION AND KALMAN ON TEST / REAL DATA #################
#########################################################################################

### Plot rmse vs. order for a variety of orders, 2 plots one for real one for test. 


##########################################################################
# Lasso #################################################################
      