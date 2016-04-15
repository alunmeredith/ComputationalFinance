# Read data into a tidy format ############################################################
library(stringr)

# Read file names
file_names <- list.files("../Data", full.names = T)
names <- str_extract(file_names, "[cp][0-9]{4}")

# Build a list of data frames for each option
daily_prices <- list()
for (ii in seq_along(file_names)) {
    tmp <- read.table(file_names[ii],
                      col.names = c("date", "option_price", "asset_value"))
    # Changed date to number of days from start indexed at 0
    tmp$date <- tmp$date - min(tmp$date)
    tmp$id <- names[ii]
    daily_prices[[names[ii]]] <- tmp
}

# Merge together
tidy_data <- Reduce(function(...) rbind(...), daily_prices)
tidy_data$id <- as.factor(tidy_data$id)

# Parse the file names to assign put/call and strike price attributes
tidy_data <- mutate(tidy_data, putcall = str_extract(id, "[cp]"),
                    strike_price = as.numeric(str_extract(id, "[0-9]{4}"))
)

###########################################################################################
# Create wide dataset and vectors for the date/asset (because shared with all options) ####

library(tidyr)
data_wide <- tidy_data %>%
    select(-c(putcall, strike_price)) %>%
    spread(id, option_price)

asset <- data.frame(date = data_wide$date,
                   value = data_wide$asset_value)

t_mat <- length(asset$date) + 1
##########################################################################
# Find the log returns, volatilities (with standard error) ###############

# Find subset of date in training set
# index of evaluation set (t > t_mat/4) i.e. latter 3/4 of data
t_quarter <- ceiling(t_mat/4)

# Specify the minimum date in time window 
asset$lag_date <- asset$date - t_quarter
ind <- asset$lag_date < 0
asset$lag_date[ind] <- NA

# Log returns
asset$log_returns <-  c(NA, diff(log(asset$value)))

# Volatilities and standard error along window 
volatilities_raw <- apply(asset, 1, function(x) {
    
    # Account for gaps of up to 4 days
    if (!(x[["lag_date"]] %in% asset$date)) {
        for (i in 1:4) {
            if ((x[["lag_date"]] + i) %in% asset$date) {
                x[["lag_date"]] <- x[["lag_date"]] + i
                break
            }
            if (i == 4) return(list(volatility = NA, volatility_se = NA))
        }
    }

    # Compute time window
    min <- which(asset$date == x[["lag_date"]])
    max <- which(asset$date == x[["date"]])
    window <- asset$log_returns[min:max]
    
    #
    s <- sd(window, na.rm = T)
    tau <- ((asset$date[max] - asset$date[min]) / length(window)) / 365
    vol <- s / sqrt(tau) # volatility
    vol_se <- vol / sqrt(2 * length(window)) # standard error
    return(list(volatility = vol, volatility_se = vol_se))
})

# Combine volatility output into asset dataframe (and wide option values)
volatilities <- volatilities_raw %>%
    unlist() %>%
    matrix(nrow = 2) %>%
    t() %>%
    as.data.frame()
names(volatilities) <- c("volatility", "volatility_se")
asset <- bind_cols(asset, volatilities)
asset <- left_join(asset, data_wide)
tidy_data <- full_join(select(asset, date, volatility, volatility_se), tidy_data)

train_ii <- !is.na(asset$volatility)
save(asset, tidy_data, train_ii, file = "tidy_data.RData")

