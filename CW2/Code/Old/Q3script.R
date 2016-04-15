tidy_data <- readRDS("tidy_data.R")
source("black_scholes_formula.R")
t_mat <- 320

# Compute implied volatilities ####
working_df <- select(tidy_data, putcall, observed_value, asset_price, strike_price, date)

implied <- vector()
for (i in 1:nrow(working_df)) {
    implied[i] <- Wrapper(working_df[i,])
}

tidy_data$implied_sigma <- implied

saveRDS(tidy_data, "tidy_data2.R")

# Make sure error is less than 0.0001 
mutate(tidy_data, est_val_2 = Putcall(putcall, asset_price, strike_price, 0.06, implied_sigma, 320, date)) -> tidy_data
index <- abs(tidy_data$est_val_2 - tidy_data$observed_value) > 0.0001
tidy_data$implied_sigma[index] <- NA

# Plot estimated vs observed for each stock
library(ggplot2)
library(ggthemes)
gg <- 
    ggplot(data = tidy_data,
           mapping = aes(x = implied_sigma, y = volatility, colour = date)) +
    geom_point() + 
    facet_wrap(~id, nrow = 2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    lims(x = c(0,.5)) +
    labs(x = "Implied Volatility", y = "Historic Volatility")
gg
# Are there systematic differences between options of different strike prices on any particular day. "Volatility Smile".
volatility_smile <- 
    tidy_data %>%
    group_by(id) %>%
    summarise(av_implied = mean(implied_sigma, na.rm = TRUE), 
              strike = mean(strike_price), 
              putcall = sample(putcall, 1),
              se_implied = sd(implied_sigma, na.rm = TRUE) / sqrt(n()))

gg <- 
    ggplot(data = volatility_smile, mapping = aes(x = strike, y = av_implied)) +
    geom_point() +
    geom_errorbar(aes(ymin = av_implied - se_implied, ymax = av_implied + se_implied)) +
    facet_wrap(~putcall, nrow = 2, scales = "free") +
    labs(x = "Strike Price", y = "Implied Volatility")
gg

### Compare implied volatility of different strike prices to 2925 strike price on each day ####
x <- vector()
for (i in 1:nrow(tidy_data)) {
    ii <- which(tidy_data$strike_price == 3125 & 
                    tidy_data$putcall == tidy_data[i, "putcall"] & 
                    tidy_data$date == tidy_data[i, "date"])
    x[i] <- tidy_data[i,"implied_sigma"] - tidy_data[ii, "implied_sigma"]
}
tidy_data$vol_smile <- x

volatility_smile <- group_by(tidy_data, id) %>%
    summarise(av_implied = mean(vol_smile, na.rm = T), strike = mean(strike_price), se_implied = sd(vol_smile, na.rm = T) / sqrt(n()), putcall = sample(putcall, 1))

gg <- ggplot(data = volatility_smile, 
             mapping = aes(x = strike, y = av_implied))
gg + geom_point() +
    geom_errorbar(aes(ymin = av_implied - se_implied, ymax = av_implied + se_implied)) +
    facet_wrap(~putcall, nrow = 2, scales = "free") +
    labs(x = "Strike Price", y = "Daily Difference in Implied Volatility, Relative to 3125")

