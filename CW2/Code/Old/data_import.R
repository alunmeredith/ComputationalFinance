# Reading in data ##############
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
    daily_prices[[names[ii]]] <- tmp
}

# Parse the file names to assign put/call and strike price attributes
putcall <- str_extract(file_names, "[cp]")
strike_price <- as.numeric(str_extract(file_names, "[0-9]{4}"))

# Bring together into one data frame
option_data <- data.frame(putcall = putcall, strike_price = strike_price)
option_data$daily_prices <- daily_prices

# cleanup
rm(list = c("names", "putcall", "ii", "file_names", "strike_price", "daily_prices", "tmp"))
