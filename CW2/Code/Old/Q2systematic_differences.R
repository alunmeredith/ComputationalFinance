source("Q2call_estimation.R")

nam <- names(volatilities)
sp <- option_data$strike_price
pc <- option_data$putcall
# Data cleaning ####
for (index in 1:length(volatilities)) {
    # Add variable for strike price
    volatilities[[index]]$strike_price <- sp[index]
    # Add variable for put / call 
    volatilities[[index]]$putcall <- pc[index]
    # Add variable for id
    volatilities[[index]]$id <- nam[index]
}

# Merge together
tidy_data <- Reduce(function(...) rbind(...), volatilities)
tidy_data$id <- as.factor(tidy_data$id)
library(dplyr)
tidy_data <- mutate(tidy_data, residual = observed_value - estimated_value)

# Plot estimated vs observed for each stock
library(ggplot2)
library(ggthemes)
gg <- ggplot(data = tidy_data, 
             mapping = aes(x = observed_value, y = estimated_value, colour = date))
gg + geom_point() + 
    facet_wrap(~ id, nrow = 2) +
    geom_abline(intercept=0, slope=1, linetype="dotted") +
    lims(x = c(0,450), y = c(0,450)) +
    labs(x = "Observed Option Price", y = "Estimated Option Price")


rm(list = setdiff(ls(), c("option_data", "volatilities", "tidy_data")))

saveRDS(tidy_data, "tidy_data.RDS")
