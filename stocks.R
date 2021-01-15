library(fpp2)
library(quantmod)

Sys.Date() # TODAY'S DATE

#apple stocks from yahoo website into APPL data table
getSymbols(Symbols = "AAPL", src = "yahoo",auto.assign = TRUE)
class(AAPL)

#create apple data frame from AAPL
apple=data.frame(AAPL)
View(apple)

#take only the closing data from apple
apple_close = apple$AAPL.Close
View(apple_close)

#convert it to time series, monthly
apple_close <- ts(data=apple_close, frequency=12, start=c(2007,1), end=c(2019,11))

#Time series plot of apple closing plot
autoplot(apple_close) + xlab("Year") + ylab("Closing Price of apple")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +ggtitle("Timeseries plot of APPLE closing price") 

#Seasonal plot
ggseasonplot(apple_close, year.labels = TRUE, year.labels.left = TRUE) + ylab("Closing price") + xlab(" Dates across year")

#
qplot(AAPL$AAPL.Close, AAPL$AAPL.Open, data=apple)


library(GGally)

ggpairs(apple[,1:5])

#forecast for next 5 months using meanf 
meanf(apple_close, h=5)

#forecast for next 5 months using naive method
naive(apple_close, h=5)

#forecast for next 5 months using snaive method
snaive(apple_close, h=5)

#Simple Linear Regression
#create a new data frame with Close, Open and High stock values
new_df= data.frame(apple$AAPL.Close,apple$AAPL.Open, apple$AAPL.High)

#Linear regression using lm function , Apple Close is dependent on apple High 
fit = lm(apple.AAPL.Close ~ apple.AAPL.High, data = new_df )
print(summary(fit))
#plot the linear regression model
ggplot( fit , aes (x= apple.AAPL.High , y= apple.AAPL.Close)) + geom_point ( color ='purple ')+
  geom_smooth( method = "lm") + xlab("Apple High Stocks") + ylab("Apple Close Stocks") + ggtitle("Linear Regression Plot of Apple Stocks")

#Trend Cycle Component

fit1 = stl(apple_close, s.window = 5)
plot(apple_close, col="gray", main="Trend cycle component", ylab="Price", xlab="Year")
lines(fit1$time.series[,2], col="red", ylab="trend")

#Moving Average

plot(apple_close, main="Apple stock closing price", ylab="Price (in Dollars)", xlab="Year")
lines(ma(apple_close,100),col="blue")
fit<-fitted(apple)
B=Arima(fit, order = c(0,0,2))
plot(B)

#ARIMA Model
#Find which order AR to use
ndiffs(apple_close)
#Apply AR(1) on apple_close
Arima(apple_close, order =  c(2,0,0))

#Libraries for plotting
library(ggplot2)
library(gridExtra)

p1 = ggplot(apple, aes(AAPL.Open)) + geom_histogram(bins = 50, aes(y = ..density..), col = "blue", fill = "red", alpha = 0.3) + geom_density()

p2 = ggplot(apple, aes(AAPL.High)) + geom_histogram(bins = 50, aes(y = ..density..), col = "blue", fill = "red", alpha = 0.3) + geom_density()

p3 = ggplot(apple, aes(AAPL.Low)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

p4 = ggplot(apple, aes(AAPL.Close)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()

grid.arrange(p1,p4,p2,p3, nrow=2,ncol=2)



