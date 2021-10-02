### Hoya 7 Time Series Project - working file
#### Please write your code under the corresponding section 

#libraries
library(ggplot2)
library(forecast)
library(grid)
library(Amelia)
library(tseries)
library(scales)
library(gridExtra)
library(lmtest)
library(Rcpp)

# read data
tsdata <- read.csv('~/Desktop/Georgetown/Marketing Analytics/usliquorsales.csv'
                   ,header =TRUE,stringsAsFactors = F)
summary(tsdata)
names(tsdata)
head(tsdata)

########## 1. Data Exploration

#Missing data check
sum(is.na(tsdata))
# no missing data

#Convert the date field from character to date type
tsdata$Period <- as.Date(tsdata$Period, format="%m/%d/%y")
tsdata$Value <- as.numeric(tsdata$Value)
summary(tsdata)
head(tsdata)
#exploratory plot
# consistent uptrend. looks like people are drinking more
ts_plot <- ggplot(tsdata, aes(Period,Value)) + geom_line(na.rm=TRUE) + 
  xlab("Time") + ylab("US Liquor Sales $ (Millions) ") + 
  scale_x_date(labels = date_format(format= "%Y"),breaks = date_breaks("1 year")) + 
  stat_smooth(colour = "green")
ts_plot


#conver to ts
cleaned_tsdata <-ts(tsdata[,c('Value')])
?ts
class(cleaned_tsdata)
summary(cleaned_tsdata)

#plot with trendline
plot(cleaned_tsdata, col = "blue", main = "US Liquor Sales Over Time ($ Millions)")
# plot(cleaned_tsdata, col = "blue")
abline(reg=lm(cleaned_tsdata~time(cleaned_tsdata)), col="lightgray") #plotting the trend line

#Test of stationary
adf.test(cleaned_tsdata)
# p-value is greater than 0.05 --> data is not stationary

# Using differencing to make data stationary



#Autocorrelation and Partial Autocorrelation Plots
Acf(cleaned_tsdata)
Pacf(cleaned_tsdata)

#Lag plot of Data
gglagplot(cleaned_tsdata, set.lags=1:16) # I think 12 looks the best 

#If the result is a small p-value than it indicates the data are probably not white noise.
#Ljung-Box test on the first 24 Lag autocorrelations

Box.test(cleaned_tsdata, lag=24, fitdf=0, type="Lj") # small p-value, confirms we need to stationarize the series 

########## 2. Stationarize the series
#Converting data into a time series object by year
alc_tsd <-ts(tsdata[,c('Value')], frequency=12)
class(alc_tsd)

#For multiplicative decomposition use the following code
component.tsm2 = decompose(alc_tsd, type="multiplicative", filter=NULL)
plot(component.tsm2)

# component.tsm2 = decompose(log(cleaned_tsdata))
# plot(component.tsm2)

#If you are using multiplicative decomposition
#In all subsequent modeling purposes, use the log of the variable

#We can remove the trend simply by differencing the data
alc_dif <-diff(cleaned_tsdata)
autoplot(alc_dif)
adf.test(alc_dif) # now we have a very small p value --> data is stationary 
Box.test(alc_dif, lag=24, fitdf=0, type="Lj") # small p-value, confirms we need to stationarize the series

#Once we’ve differenced we’ve effectively removed the trend from our data and can reapply the SES model
alc_ses2 <-ses(alc_dif, alpha=0.2, h=24)
autoplot(alc_ses2)


########## 3. Plot ACF/PACF charts and find optimal parameters
# I am now using the difference data 
Acf(alc_dif)
Pacf(alc_dif)

########## 4. Build the ARIMA model
dfit1 <-arima(cleaned_tsdata, order=c(0,1,0))
plot(residuals(dfit1))

Acf(residuals(dfit1))
Pacf(residuals(dfit1))

#Because the seasonal pattern is strong and stable, 
#we will want to use an order of seasonal differencing in the model. 
#Before that let’s try only with one seasonal difference i.e ARIMA(0,0,0)(0,1,0)

dfit2 <- arima(cleaned_tsdata, order =c(0,0,0), seasonal = list(order = c(0,1,0), period = 12))
plot(residuals(dfit2))
Acf(residuals(dfit2))
Pacf(residuals(dfit2))

#lets try and apply both seasonal and non-seasonal differencing, ARIMA(0,1,0)(0,1,0)[12]
dfit3 <- arima(cleaned_tsdata, order =c(0,1,0), seasonal = list(order = c(0,1,0), period = 12))
plot(residuals(dfit3))
Acf(residuals(dfit3))
Pacf(residuals(dfit3))


########## 5. Make predictions