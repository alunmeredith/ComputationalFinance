setwd("~/Southampton/Term2/ComputationalFinance/CW1/Data")
library(dplyr)
filenames <- list.files()

#df = data.frame(Date = seq(as.Date("2015-02-18"), as.Date("2016-02-17"), 1))
#df$Date <- as.character(df$Date)

df = data.frame(Date = character())

for(file in filenames){
    temp = read.csv(file, stringsAsFactors = F)
    temp <- temp %>% select(Date, Close)
    names(temp) <- c("Date", file)
    df <- full_join(df, temp)
}

# Remove days where the market was closed
dates_closed <- c("2016-01-01", "2015-04-03", "2015-04-06", "2015-05-04", "2015-05-25", "2015-08-31", "2015-12-25", "2015-12-28", "2016-02-18", "2015-01-01", "2014-01-01", "2014-03-29", "2014-04-01", "2014-05-02", "2014-05-06", "2014-08-26", "2014-12-25", "2014-12-26", "2013-01-01", "2013-03-29", "2013-04-01", "2013-05-06", "2013-05-27", "2013-08-26", "2013-12-25", "2013-12-26")
df %>% filter(!(Date %in% dates_closed)) -> df

# Remove all the stock which have more than 45 NA values (5 values)
index <- apply(df, 2, function(x) sum(is.na(x))) > 40
df <- df[,!index]
# Remove some anomalous findings (RSA stock graph looks has a weird 5x overnight jump in price which isn't shown online, and RR is actually NA values for almost all data encoded as 0s. 
df %>% select(-c(RSA.csv, RR.csv, BLT.csv, III.csv)) -> df

# Impute remaining NA values (3 stocks with 5, 4 and 5 NA values)
# For IMT the na values are the first 4 observations, in this case we impute with the same average daily change of the next 30 days. 
dailyChange_IMT = (df$IMT.csv[35] - df$IMT.csv[5])/ 35
df$IMT.csv[1:4] <- seq(df$IMT.csv[5] - dailyChange_IMT, by = -dailyChange_IMT, length.out = 4)

# For III NA values are 1 off missing data, so impute with the average of the surrounding datapoints. 

i = 1
while(sum(is.na(df$III.csv)) > 0){
    index <- which(is.na(df$III.csv))
    df$III.csv[index] <- (df$III.csv[index + i] + df$III.csv[index - i])/2
    index <- which(is.na(df$III.csv))
    i = i + 1
}
i = 1
while(sum(is.na(df$FTSE.csv)) > 0){
    index <- which(is.na(df$FTSE.csv))
    df$FTSE.csv[index] <- (df$FTSE.csv[index + i] + df$FTSE.csv[index - i])/2
    index <- which(is.na(df$FTSE.csv))
    i = i + 1
}

# Select 30 stocks randomly (and FTSE)
df[,c("Date", sample(names(select(df, -FTSE.csv, -Date)), 30), "FTSE.csv")] -> df

write.csv(df, "./../joined_stocks.csv")
write.csv(filenames, "./../filenames.csv")