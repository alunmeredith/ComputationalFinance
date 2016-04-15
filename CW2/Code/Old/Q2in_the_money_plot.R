tidy_data <- mutate(tidy_data, 
                    d1 = D1(asset_price, strike_price,0.06, volatility, (319 - date)/365),
                    d2 = D2(asset_price, strike_price,0.06, volatility, (319 - date)/365),
                    b = K * exp(-0.06 * (319 - date)/365),
                    inthemoney = pnorm(d1),
                    temp = if(putcall=="p") 1 else (0),
                    inthemoney2 = abs(inthemoney -temp))

tidy_data$value_less_than_one <- tidy_data$estimated_value < 1

library(dplyr)
tidy_data %>% group_by(index, putcall) -> tidy_data
summarise(tidy_data, mean(volatility, na.rm = T))

ggplot(tidy_data, aes(x = date, y = inthemoney2, colour = value_less_than_one)) +
    geom_point() +
    facet_wrap(~ id, nrow = 2) +
    labs(x = "date", y = "Probability of 'in the money'", colour = "Asset Value < 1")
