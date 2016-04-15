library(stringr)
source("black_scholes_formula.R") # Import functions
source("data_import.R") # Read data

# Define dates (same for every stock)
dates <- option_data$daily_prices[[1]]$date
t_mat <- max(dates) + 2 # counting 0 indexing and t_mat is day after series ends

# Find subset of dates in training set
# index of evaluation set (t > t_mat/4) i.e. latter 3/4 of data
t_quarter <- ceiling(t_mat/4)
ii_train <- which(dates > t_quarter)

# Initialise data frame for results 
# each column volatilities (or errors) for a different option
volatility_df <- data.frame(Date = dates[ii_train])
volatility_error_df <- data.frame(Date = dates[ii_train])

# Loop through each option ####
n <- 1
volatilities <- list()
for (n in seq_len(nrow(option_data))) {
    daily_prices <- option_data[[n,"daily_prices"]]
    
    # Estimate volatility through sliding window ####
    # Consider T/4 before the current date (in time not number of observations)
    min_dates <- daily_prices$date - t_quarter
    vol_call_df <- sapply(ii_train, function(ii) {
        
        # Find training subset rows inside the date range
        window_ii <- (daily_prices$date >= min_dates[ii]) & 
                     (daily_prices$date < daily_prices$date[ii])
        vol_train <- daily_prices[window_ii,]
        vol_train$returns <- log(lag(vol_train$asset_value)) - log(vol_train$asset_value)
        
        # Calculate volatility and volatility error
        s   <- sd(vol_train$returns, na.rm = T) # includes bessels correction (n-1)
        tau <- (max(vol_train$date) - min(vol_train$date))/365 # portion of year estimated over
        sigma <- s / sqrt(tau) # volatility
        sigma_err <- sigma / sqrt(2 * nrow(vol_train)) # standard error
        
        observed_value <- daily_prices$option_price[ii]
        # Variables for estimating call
        t_cur <- dates[ii] # current time
        r <- 0.06 # interest rate
        S <- daily_prices$asset_value[ii] #asset price
        K <- option_data$strike_price[n] #strike price
        ifelse(option_data$putcall[n] == "c", 
              estimated_value <- Call(S,K,r,sigma,t_mat,t_cur),
              estimated_value <- Put(S,K,r,sigma,t_mat,t_cur))
        asset_price <- daily_prices$asset_value[ii]
        return(c(dates[ii], sigma, sigma_err, observed_value, estimated_value, asset_price))
    })
    vol_call_df <- t(vol_call_df)
    colnames(vol_call_df) <- c("date", "volatility", "volatility_error", "observed_value", "estimated_value", "asset_price")
    vol_call_df <- as.data.frame(vol_call_df)
    
    name <- names(option_data$daily_prices)[n]
    volatilities[[name]] <- vol_call_df
}

# clean up
rm(list = setdiff(ls(), c("option_data", "volatilities")))
