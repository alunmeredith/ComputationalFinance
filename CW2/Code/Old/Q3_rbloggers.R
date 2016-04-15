## Black-Scholes Function
BS <-
    function(S, K, T, r = 0.06, sig, type="c"){
        d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
        d2 <- d1 - sig*sqrt(T)
        if(as.character(type) =="c"){
            value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
        }
        if(as.character(type) =="p"){
            value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
        }
        return(value)
    }


## Function to find BS Implied Vol using Bisection Method
implied.vol <-
    function(S, K, T, r, market, type){
        sig <- 0.20
        sig.up <- 1
        sig.down <- 0.001
        count <- 0
        err <- BS(S, K, T, r, sig, type) - market 
        
        ## repeat until error is sufficiently small or counter hits 1000
        while(abs(err) > 0.00001 && count<1000){
            if(err < 0){
                sig.down <- sig
                sig <- (sig.up + sig)/2
            }else{
                sig.up <- sig
                sig <- (sig.down + sig)/2
            }
            err <- BS(S, K, T, r, sig, type) - market
            count <- count + 1
        }
        
        ## return NA if counter hit 1000
        if(count==1000){
            return(NA)
        }else{
            return(sig)
        }
    }

dat <- readRDS("tidy_data.R")


## calculate implied vol for Call
T.max <- 320
r <- 0.06

n <- dim(dat)[1]
vol <- rep(0,n)

for(i in 1:n){
    vol[i] <- implied.vol(dat$asset_price[i], dat$strike_price[i], (T.max - dat$date)/365, r, dat$observed_value[i], as.character(dat$putcall[i]))
}

dat$vol <- vol

## plot Volatility Smile and Open Interest
volatility_smile <- 
    dat %>%
    group_by(id) %>%
    summarise(av_implied = mean(vol, na.rm = TRUE), 
              strike = mean(strike_price), 
              putcall = sample(putcall, 1),
              se_implied = sd(vol, na.rm = TRUE) / sqrt(n()))

gg <- 
    ggplot(data = volatility_smile, mapping = aes(x = strike, y = av_implied)) +
    geom_point() +
    geom_errorbar(aes(ymin = av_implied - se_implied, ymax = av_implied + se_implied)) +
    facet_wrap(~putcall, nrow = 1, scales = "free") +
    labs(x = "Strike Price", y = "Implied Volatility")
gg
