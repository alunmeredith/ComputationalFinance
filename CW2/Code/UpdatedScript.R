load("tidy_data.RData")
source("helper_functions.R")
library(dplyr)

# Set global variables
r = 0.06
t_mat = max(asset$date) + 1

#####################################################
### Question 2 - Estimating option value ############
#####################################################

tidy_data <- mutate(tidy_data, 
               option_estimate = Putcall(putcall = putcall,
                                 S = asset_value,
                                 K = strike_price,
                                 r = r,
                                 sigma = volatility,
                                 t_cur = date,
                                 t_mat = t_mat
               )
)

#####################################################
lambda <- function(S, K, r, volatility, date, putcall){
    D1 <- function(S,K,r,sigma,t) {
        denom <- sigma * sqrt(t)
        a <- log(S/K)
        b <- (r + sigma^2 / 2) * t
        return((a + b) / denom)
    }
    d1 = D1(S, K, r, volatility,(319 - date)/365)
    ret <- pnorm(d1)
    ret <- ifelse(putcall == "p", pnorm(d1), 1 - ret)
    return(ret)
}
tidy_data <- mutate(tidy_data, in_the_money = lambda(S = strike_price,
                                                     K = asset_value,
                                                     r = r,
                                                     volatility = volatility,
                                                     date = date,
                                                     putcall = putcall)
)
#####################################################
### Question 3 - Implied Volatility ################
####################################################

tidy_data <- mutate(tidy_data,
                    vol_implied = Implied(
                        putcall = putcall,
                        Val_obs = option_price,
                        S = asset_value,
                        K = strike_price,
                        r = r,
                        t_cur = date)
)

vol_implied <- vector()
for (i in 1:nrow(tidy_data)) {
    vol_implied[i] <- Implied(putcall = tidy_data[[i, "putcall"]],
            Val_obs = tidy_data[[i, "option_price"]],
            S = tidy_data[[i, "asset_value"]],
            K = tidy_data[[i, "strike_price"]],
            r = r,
            t_cur = tidy_data[[i, "date"]],
            interval = c(0, 100),
            t_mat = t_mat)
}

tidy_data$vol_implied <- vol_implied

#####################################################
##### Question 4 Volatility Smile ###################
#####################################################

# Using fOption package because my estimates do not produce a good volatility smile for whatever reason, fOptions file simply uses packages to estimate volatility and Black Scholes option estimates. 
source("fOptions.R")
volatility_smile <- 
    tidy_data %>%
    group_by(id) %>%
    summarise(av_implied = mean(fOption_implied, na.rm = TRUE), 
              strike = mean(strike_price), 
              putcall = sample(putcall, 1),
              se_implied = sd(fOption_implied, na.rm = TRUE) / sqrt(n()))

#####################################################
#### Question 5 - Binomial vs Black Scholes #########
#####################################################

# Consider 1 option with a random set of values (all others fixed)

# Take fixed values from a random option in our dataset
option <- tidy_data[sample(nrow(tidy_data),1),]
# Randomly sample many volatilities from dataset (with replacement 10,000 times - na values)
vols <- tidy_data$volatility[sample(nrow(tidy_data), 1000, replace = T)] %>% 
    na.omit
binomial_steps = 100
BS <- matrix(ncol = binomial_steps, nrow = length(vols))
Bin <- matrix(ncol = binomial_steps, nrow = length(vols))
for (n in 1:binomial_steps) {
    
    for (i in seq_along(vols)) {
    
        # Estimate black scholes for each
        BS[i,n] <- GBSOption(TypeFlag = option$putcall,
                        S = option$asset_value,
                        X = option$strike_price,
                        Time = (t_mat - option$date)/365,
                        r = r,
                        b = 0,
                        sigma = vols[i])@price
        
        Bin[i,n] <- CRRBinomialTreeOption(TypeFlag = paste0(option$putcall,"e"),
                                        S = option$asset_value,
                                        X = option$strike_price,
                                        Time = (t_mat - option$date)/365,
                                        r = r,
                                        b = 0,
                                        sigma = vols[i],
                                        n = n
                                        )@price
    }
}
Abs_diff <- Bin - BS
bin_v_bs <- list(option = option, vols = vols, Binomial = Bin, BlackScholes = BS, differences = Abs_diff)

#######################################################
# Real data contrast 
for (i in c(.1, .125, .25, .5)) {
    x <- vector()
    for (ii in 1:nrow(tidy_data)) {
        dt = ceiling((t_mat - tidy_data[[ii,"date"]]) * i)
        print(dt)
        x[ii] <- with(tidy_data[ii,], 
                  CRRBinomialTreeOption(TypeFlag = paste0(putcall,"e"), 
                                        S = asset_value, 
                                        X = strike_price, 
                                        Time = (t_mat - date)/365, r = r,
                                        b = 0, sigma = volatility, 
                                        n = dt)
                  )@price
    }
    name <- paste0("binom",i)
    tidy_data[,name] <- x
    print(name)
    save(tidy_data, volatility_smile, asset, train_ii, bin_v_bs, file = "tidy_data2.RData")
}

#####################################################
#### Question 6 - American Vs European Binomial #####
#####################################################
# Using dt = 1 day
x <- vector()
y <- vector()
for (ii in 1:nrow(tidy_data)) {
    x[ii] <- with(tidy_data[ii,], 
                  CRRBinomialTreeOption(TypeFlag = paste0(putcall,"a"), 
                                        S = asset_value, 
                                        X = strike_price, 
                                        Time = (t_mat - date)/365, r = r,
                                        b = 0, sigma = volatility, 
                                        n = (t_mat - date))
    )@price
    y[ii] <- with(tidy_data[ii,], 
                  CRRBinomialTreeOption(TypeFlag = paste0(putcall,"e"), 
                                        S = asset_value, 
                                        X = strike_price, 
                                        Time = (t_mat - date)/365, r = r,
                                        b = 0, sigma = volatility, 
                                        n = (t_mat - date))
    )@price
}
i <- 1
name <- paste0("binom",i, "_american")
tidy_data[,name] <- x
name <- paste0("binom",i)
tidy_data[,name] <- y


save(tidy_data, volatility_smile, asset, train_ii, bin_v_bs, r, t_mat, file = "tidy_data_fin.RData")
readr::write_csv(tidy_data, "tidy_data_fin.csv")
