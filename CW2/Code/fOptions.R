library(fOptions)
library(dplyr)
#load("tidy_data.RData")

t_mat = max(asset$date) + 1
r = 0.06

temp <- vector()
for (ii in 1:nrow(tidy_data)) {
    temp[ii] <- GBSOption(
        TypeFlag = tidy_data[ii,]$putcall,
        S = tidy_data[ii,]$asset_value,
        X = tidy_data[ii,]$strike_price,
        Time = (t_mat - tidy_data[ii,]$date) / 365,
        r = r,
        sigma = tidy_data[ii,]$volatility,
        b = 0
    )@price
}
tidy_data$fOption_estimate <- temp

# Implied volatility
implied <- vector()
for (i in 1:nrow(tidy_data)) {
    implied[i] <- GBSVolatility(
        price = tidy_data[[i,"option_price"]],
        TypeFlag = as.character(tidy_data[[i, "putcall"]]),
        S = tidy_data[[i, "asset_value"]],
        X = tidy_data[[i, "strike_price"]],
        Time = (t_mat - tidy_data[[i, "date"]]) / 365,
        r = r,
        b = 0,
        tol = 0.1,
        maxiter = 1000 
        )
}
tidy_data$fOption_implied <- implied

#saveRDS(tidy_data, "tidy_data_fOptions.R")