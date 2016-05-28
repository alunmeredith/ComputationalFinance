library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
load("tidy_data_fin.RData")
#################

# Change putcall labels to full words for facets
tidy_data$putcall <- sub("^p$", "Put", tidy_data$putcall) 
tidy_data$putcall <- sub("^c$", "Call", tidy_data$putcall)
# tidy_data$putcall <- sub("^Put$", "p", tidy_data$putcall) 
# tidy_data$putcall <- sub("^Call$", "c", tidy_data$putcall)
volatility_smile$putcall <- sub("^p$", "Put", volatility_smile$putcall) 
volatility_smile$putcall <- sub("^c$", "Call", volatility_smile$putcall)

image_size <- 240

###########################
##### QUESTION 1 ##########
###########################

# Define paramaters
opt_prices = 5

# Function to calculate profit
profit <- function(x, fixed_cost = opt_prices) {
    ifelse(x < 40,
           40 - x - fixed_cost, 
           ifelse(x > 45,
                  x -  45 - fixed_cost,
                  - fixed_cost))
}

# Plot
range <- data.frame(x = 30:55)
gg <- ggplot(range, aes(x)) +
    stat_function(fun = profit, size = 1.5) +
    labs(x = "Final Stock Price", y = "Profit") +
    geom_vline(xintercept = 40, linetype = 2, alpha = 0.5) +
    geom_vline(xintercept = 45, linetype = 2, alpha = 0.5) +
    annotate("text", x = mean(c(min(range), 40)), y = 4.5, 
             label = "Put option \n in the money") +
    annotate("text", x = mean(c(max(range), 45)), y = 4.5, 
             label = "Call option \n in the money") +
    theme_few()

jpeg(file = "../Plots/Q1_1.jpg", width = image_size, height = image_size)
gg
dev.off()

###########################
##### QUESTION 2 ##########
###########################

# Graph 1 - Plot underlying asset + historic volatility
#######################################################
dat <- dplyr::filter(tidy_data, id == "c3125") %>%
    dplyr::select(date, volatility, asset_value) %>%
    gather(value, stat, -date)
gg <- ggplot(data = dat, aes(x = date, y = stat)) + 
    geom_line() + 
    facet_wrap(~value, nrow = 2, scales = "free_y") +
    ylab("") +
    ggthemes::theme_few()
gg

jpeg(file = "../Plots/Q2_1.jpg", width = image_size, height = image_size)
gg
dev.off()



# Plot estimated vs observed for each stock
gg <- ggplot(tidy_data, aes(x = date, y = fOption_estimate - option_price, )) +
    geom_line() +
    facet_wrap(~id, nrow = 2) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    #lims(x = c(0,450), y = c(0,450)) +
    labs(x = "Observed Option Price", y = "Estimated Option Price") + 
    theme_few() + 
    scale_colour_continuous(low = "black")
gg

jpeg(file = "../Plots/Q2_2.jpg", width = image_size*2, height = image_size)
gg
dev.off()

# Summary data
se <- function(x) sqrt(var(na.omit(x))/length(na.omit(x)))
sum_dat <- tidy_data %>% group_by(id) %>% 
    summarise(err = mean(fOption_estimate - option_price, na.rm = T),
              err_se = se(fOption_estimate - option_price),
              putcall = sample(putcall, 1),
              strike = sample(strike_price, 1))

gg <- ggplot(sum_dat, aes(x = strike, y = err)) +
    geom_errorbar(aes(ymin = err - 2*err_se, ymax = err + 2*err_se), width = 20) +
    geom_point() +
    facet_wrap(~putcall, nrow = 1) + 
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme_few() + 
    ylab("Error") +
    xlab("Strike Price")
gg

jpeg(file = "../Plots/Q2_3.jpg", width = image_size* 2, height = image_size)
gg
dev.off()

#
gg <- ggplot(tidy_data, aes(x = date, y = in_the_money)) + 
    facet_wrap(~id, nrow = 2) + 
    geom_line() +
    theme_few() +
    ylab("probability in the money")
gg

jpeg(file = "../Plots/Q2_4.jpg", width = image_size* 3, height = image_size)
gg
dev.off()
###########################
##### QUESTION 3 ##########
###########################

# Plot estimated vs observed for each stock
gg <- 
    ggplot(data = tidy_data,
           mapping = aes(x = volatility, y = fOption_implied, colour = date)) +
    geom_point() + 
    facet_wrap(~id, nrow = 2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    xlim(c(0,.3)) + ylim(c(0,.2)) +
    labs(x = "Historic Volatility", y = "Implied Volatility") +
    theme_few()
gg

jpeg(file = "../Plots/Q3_1.jpg", width = image_size* 2, height = image_size)
gg
dev.off()

# Are there systematic differences between options of different strike prices on any particular day. "Volatility Smile".
gg <- 
    ggplot(data = volatility_smile, mapping = aes(x = strike, y = av_implied, colour = putcall)) +
    geom_point() +
    geom_errorbar(aes(ymin = av_implied - 2*se_implied, ymax = av_implied + 2*se_implied)) +
    #acet_wrap(~putcall, nrow = 2, scales = "free") +
    labs(x = "Strike Price", y = "Implied Volatility") +
    theme_few() +
    scale_color_grey()
gg

jpeg(file = "../Plots/Q3_2.jpg", width = image_size * 1.5, height = image_size)
gg
dev.off()

###########################
##### QUESTION 5 ##########
###########################

# Graph 1  
###########################
# Randomly sampled (bootstrapped) volatilities with fixed other parameters comparing black scholes and binomial absolute error to show they converge as dt approaches 0. 

Abs_diff <- bin_v_bs$Binomial - bin_v_bs$BlackScholes

diff_tall <- tidyr::gather(as.data.frame(Abs_diff), n, difference)
diff_tall$n <- stringr::str_extract(diff_tall$n, "[0-9]") %>% as.numeric

t_left <- t_mat - bin_v_bs$option$date
dat <- diff_tall %>% 
    group_by(n) %>% 
    summarise(mean = mean(difference), err = se(difference)) %>%
    mutate(dt = t_left/n)

gg <- ggplot(data = dat, aes(x = dt, y = mean)) +
    geom_errorbar(aes(ymin = mean - 2*err, ymax = mean + 2*err), width = 1) +
    xlab("dt (days)") +
    ylab("Binomial - Black Scholes approximation") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ylim(-1,0.25) +
    theme_few()
gg

jpeg(file = "../Plots/Q5_1.jpg", width = image_size * 1.5, height = image_size)
gg
dev.off()

# Graph 2
###########################

gg <- ggplot(dplyr::filter(tidy_data, putcall == "Call"), 
             aes(y = fOption_estimate)) + 
    geom_boxplot(aes(y = fOption_estimate - binom0.5, x = 2), outlier.size = .2) +
    geom_boxplot(aes(y = fOption_estimate - binom0.1, x = 10), outlier.size = .2) +
    geom_boxplot(aes(y = fOption_estimate - binom0.25, x = 4), outlier.size = .2) +
    geom_boxplot(aes(y = fOption_estimate - binom0.125, x = 8), outlier.size = .2) +
    xlab("dt (days)") +
    ylab("Black Scholes - Binomial option price") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_few()
gg
    
jpeg(file = "../Plots/Q5_2.jpg", width = image_size * 1.5, height = image_size)
gg
dev.off()

###########################
##### QUESTION 6 ##########
###########################

# Graph 1
###########################
# Compare american vs European option prices using fOptions to show for each option american prices are always higher 
#100 * (binom1 - binom1_american)/binom1)
dat <- dplyr::filter(tidy_data, putcall == "Put")
gg <- ggplot(dat, aes(x = date, y = binom1_american - binom1)) + 
    facet_wrap(~id, nrow = 1) + 
    geom_line() +
    ylab("Estimated American - European Option value") +
    theme_few() +
    geom_hline(yintercept = 0, linetype = "dashed")
gg

jpeg(file = "../Plots/Q6_1.jpg", width = image_size * 4, height = image_size)
gg
dev.off()