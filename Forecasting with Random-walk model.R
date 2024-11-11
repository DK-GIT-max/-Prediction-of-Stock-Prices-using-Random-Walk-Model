##Random Walk model
#A timer series model used for stock-price predictions.Each day is called a step.
##Random walk theory states that stock prices behave in a random way. It is not based on historical data because it is random and it is based on current value
###Simple random walk model(SRWM) contains
#SRWM(Yt) = Yt-1 + Et (White noise-residual error)
#Random walk model with drfit (Yt) = Yt-1 + Mue + Et (White noise)
#Geometric Random walk model(Yt) = Tita(Yt-1+Mue+Et) where tita represents proportion


library(PerformanceAnalytics)
library(quantmod)
library(ggplot2)

stock_prices <- getSymbols("LTIM.NS", from='2020-01-01', to='2024-11-10', auto.assign=FALSE)
stock_prices <- Cl(stock_prices) #taking Closing prices only

#Calculate simple returns
stock_returns <- Return.calculate(stock_prices,method = "discrete")
stock_returns <- na.omit(stock_returns) #Removing the NA values caused by the first observation

#Alternative method
#Log returns
stock_returns_log <- Return.calculate(stock_prices,method="log")
stock_returns_log <- na.omit(stock_returns_log)

set.seed(123) #For reproducibility

#Calculating mean and S.D. of simple returns
mu <- mean(stock_returns) #Mean return (drift)
sigma <- sd(stock_returns)

#Generating a random walk
n <- 100 #Forecast horizon (100 days)
random_walk <- numeric(n)#Initializing vector for simulated price
random_walk[1] <- as.numeric(last(stock_prices)) #Start from the last known price
last(stock_prices)

#Geometric Random Walk model
for (i in 2:n){
  random_walk[i] <- random_walk[i-1]*(1+mu+sigma*rnorm(1))
}

future_dates <- seq(as.Date("2024-10-02"),by="days",length.out=n)
class(future_dates)
random_walk_df <- data.frame(Date=future_dates,Price=random_walk)

ggplot(random_walk_df,aes(x=Date,y=Price))+
  geom_line(color="blue")+
  ggtitle("Random Walk Model Simulation for LTIM.NS Stock Price")+
  xlab("Date")+ylab("Price")

print(random_walk_df)



##Simple random walk with drift
#Yt = Yt-1 + Mue + epsilon

stock_prices <- getSymbols("LTIM.NS", from='2024-04-24', to='2024-08-02', auto.assign=FALSE)
stock_prices <- Cl(stock_prices) #Close prices

#Calculate simple returns
stock_returns <- Return.calculate(stock_prices,method = "discrete")
stock_returns <- na.omit(stock_returns) #Remove NA values caused by the first observation

#Alternative method
#Log returns
stock_returns_log <- Return.calculate(stock_prices,method="log")
stock_returns_log <- na.omit(stock_returns_log)

set.seed(123) #For reproducibility

#Calculate mean and S.D. of simple returns
mu <- mean(stock_returns) #Mean return (drift)
sigma <- sd(stock_returns)

#Generate random walk
n <- 100 #Forecast horizon (e.g. 100 days)
random_walk <- numeric(n)#Initialize vector for simulated price
random_walk[1] <- as.numeric(last(stock_prices)) #Start from the last known price
last(stock_prices)

#Generating simple random walk with drift model
for ( i in 2:n){
  random_walk[i] <- random_walk[i-1]+mu+(sigma*rnorm(1)) #RWM with drift i.e., drift is mean
}

#Combining the future dates and simulated prices into a data frame for easier plotting and analysis
future_dates <- seq(as.Date("2024-08-02"),by="days",length.out=n)
class(future_dates)
random_walk_df <- data.frame(Date=future_dates,Price=random_walk)

print(random_walk_df)


getSymbols("LTIM.NS", from = "2023-08-02", to = "2024-11-10", auto.assign = TRUE)
stock_prices_1 <- Cl(LTIM.NS)

# Ensure that both actual and predicted prices have the same length
actual_prices <- tail(stock_prices_1, n=length(random_walk_df$Price))
predicted_prices <- random_walk_df$Price

actual_prices
predicted_prices


mae <- mean(abs(actual_prices - predicted_prices))
mse <- mean((actual_prices - predicted_prices)^2)
rmse <- sqrt(mse)

cat("MAE:", mae, "\nMSE:", mse, "\nRMSE:", rmse)


ggplot(random_walk_df,aes(x=Date,y=Price))+
  geom_line(color="blue")+
  ggtitle("Random Walk Model Simulation for LTIM.NS Stock Price")+
  xlab("Date")+ylab("Price")

#Comparison with ARIMA model

######ARIMA Model#########
library(quantmod)
library(forecast)
getSymbols("LTIM.NS",from='2024-04-24',to='2024-08-02')
stock_prices <- Cl(LTIM.NS)
#Plot the stock prices to see the trend
plot(stock_prices,main="LTIM.NS stock prices",ylab="Price",xlab="Date")

#ARIMA stands for Auto regressive Moving Average Integration
#To handle stationary part, we have integrate auto regression and moving average
arima_model_prices <- auto.arima(stock_prices)
summary(arima_model_prices)

#Forecast for stock prices
forecast_prices <- forecast(arima_model_prices,h=100) #Forecast for 100 days
plot(forecast_prices,main="ARIMA Forecast for LTIM.NS Stock Prices")




