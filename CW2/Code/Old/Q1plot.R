library(ggplot2)
library(ggthemes)

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
ggplot(range, aes(x)) +
    stat_function(fun = profit, size = 1.5) +
    labs(x = "Final Stock Price", y = "Profit") +
    geom_vline(xintercept = 40, linetype = 2, alpha = 0.5) +
    geom_vline(xintercept = 45, linetype = 2, alpha = 0.5) +
    annotate("text", x = mean(c(min(range), 40)), y = 4.5, label = "Put option \n in the money") +
    annotate("text", x = mean(c(max(range), 45)), y = 4.5, label = "Call option \n in the money") +
    theme_few()

caption = "Graph showing the profit as a function of asset price for a combination of a call and put option on the same underlying asset with strike prices ?45 and ?50 respectively, assuming total option cost of ?5."
