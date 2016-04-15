



## Black Scholes Function
Putcall <- function(putcall,S,K,r,sigma,t_mat,t_cur) {
    t <- (t_mat - t_cur) / 365
    
    # functions to compute the subvariables d1 and d2 in closed form BS solution
    D1 <- function(S,K,r,sigma,t) {
        denom <- sigma * sqrt(t)
        a <- log(S/K)
        b <- (r + sigma^2 / 2) * t
        return((a + b) / denom)
    }
    D2 <- function(S,K,r,sigma,t) {
        d1 <- D1(S,K,r,sigma,t)
        return( d1 - sigma * sqrt(t))
    }
    d1 <- D1(S,K,r,sigma,t)
    d2 <- D2(S,K,r,sigma,t)
    
    # Ifelse statement for put vs call statement
    if (putcall == "p") {
        price <- pnorm(-d2) * K * exp(-r * t) - pnorm(-d1) * S
    }
    else if (putcall == "c") {
        price <- S * pnorm(d1) - K * exp(-r * (t)) * pnorm(d2)
    }
    else price <- NA
    return(price)
}

# Implied <- function(putcall,Val_obs,S,K,r,interval,t_mat,t_cur, level = 1) { ####
# 
#     #print(paste("interval: ", interval[1], interval[2]))
#     high <- Putcall(putcall,S,K,r,interval[1],t_mat,t_cur) - Val_obs
#     low <- Putcall(putcall,S,K,r,interval[2],t_mat,t_cur) - Val_obs
#     #print(paste("interval Values: ", high, low))
#     
#     if (high < 0 | low > 0) {
#         warning("interval does not bisect 0")
#         return(NA)
#     }
#     if (abs(high) < 0.0001 | abs(low) < 0.0001) return(mean(interval))
# 
#     midpoint <- mean(interval)
#     mid <- Putcall(putcall,S,K,r,midpoint,t_mat,t_cur) - Val_obs
#     #print(paste("midpoint: ", midpoint, " Value: ", mid))
#     
#     if (level > 1000) {
#         warning("recursion limit reached at error value: ", mid)
#         return(NA)
#     }
#     
#     ifelse(mid > 0, 
#            interval[1] <- midpoint,
#            interval[2] <- midpoint)
#     midpoint <- Implied(putcall,Val_obs, S,K,r,interval,t_mat,t_cur, level + 1)
#     return(midpoint)
# }

############

# Implied volatility function ####
Implied <- function(putcall,Val_obs,S,K,r,interval,t_cur, t_mat = 320, level = 1) { 
    
    error <- 1
    while (abs(error) > 0.0001 & level <= 1000) {
        error <- Putcall(putcall,S,K,r,mean(interval),t_mat,t_cur) - Val_obs
        ifelse(error > 0,
               interval[1] <- mean(interval),
               interval[2] <- mean(interval)
        )
        level <- level + 1
        if (interval == c(0,0)) return(NA)
        if (level == 9999) return(NA)
    }
    return(mean(interval))
}
