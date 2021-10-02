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
tsdata <- read.csv('/Users/oliverpanbiz/Documents/hoya7capstone/usliquorsales_changed_op.csv'
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
class(cleaned_tsdata)
summary(cleaned_tsdata)

#plot with trendline
#plot(cleaned_tsdata, col = "blue", main = "US Liquor Sales Over Time ($ Millions)")
plot(cleaned_tsdata, col = "blue")
abline(reg=lm(cleaned_tsdata~time(cleaned_tsdata)), col="lightgray") #plotting the trend line

#Test of stationary
adf.test(cleaned_tsdata)



#Autocorrelation and Partial Autocorrelation Plots
Acf(cleaned_tsdata)
Pacf(cleaned_tsdata)

#Lag plot of Data
gglagplot(cleaned_tsdata, set.lags=1:16)
# 12 is the strongest

#component.ts = decompose(cleaned_tsdata) 
#plot(component.ts)

component.tsm2 = decompose(cleaned_tsdata, type="multiplicative", filter=NULL)
plot(component.tsm2)


# clean data
cleaned_tsdata1 <-ts(tsdata[,c('Value')])

class(cleaned_tsdata1)
tsdata$lsales <-tsclean(cleaned_tsdata1)

#Plot the cleaned data
c_ts_plot <- ggplot(tsdata, aes(Period,lsales)) + geom_line(na.rm=TRUE) + 
  xlab("Month") + ylab(" Sales in Thousands") + 
  scale_x_date(labels = date_format(format= "%b-%Y"),breaks = date_breaks("1 year")) + 
  stat_smooth(colour="green")
c_ts_plot

#compare cleaned vs. uncleaned plots
grid.arrange(ts_plot,c_ts_plot,ncol=1, top = textGrob("Uncleaned vs Cleaned Series"))


#plot
c_ts_plot <- ggplot(tsdata, aes(Period,lsales)) + geom_line(na.rm=TRUE) +
  xlab("Month") + ylab("Liquor Sales in Thousands") +
  scale_x_date(labels = date_format(format= "%b-%Y"),breaks = date_breaks("1 year")) + stat_smooth(colour="green")
c_ts_plot

#We can remove the trend simply by differencing the data
alc_dif <-diff(cleaned_tsdata1)
autoplot(alc_dif)

Acf(alc_dif)
Pacf(alc_dif)


#Use the cleaned data set and check acf and pacf plots again
my_ts <- ts(na.omit(tsdata$lsales), frequency = 12)
plot(my_ts)

component.ts2 = decompose(my_ts)
plot(component.ts2)

Acf(my_ts)
Pacf(my_ts)
#this didn't seem to do a whole lot


########## 4. Build the ARIMA model, from Dani's code
dfit1 <-arima(cleaned_tsdata, order=c(0,1,0))
plot(residuals(dfit1))

Acf(residuals(dfit1))
Pacf(residuals(dfit1))
summary(dfit1)
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



dfit5 <- arima(cleaned_tsdata1, order =c(1,1,0), 
               seasonal = list(order = c(0,1,1), period = 12))
plot(residuals(dfit5)) 
Acf(residuals(dfit5)) 
Pacf(residuals(dfit5))

summary(dfit5)


coeftest(dfit5)


dfit6 <- auto.arima(cleaned_tsdata)
summary(dfit6)

dfit7 = arima(cleaned_tsdata1, order =c(2,1,2), seasonal = list(order = c(0,2,2), period = 12))
summary(dfit7)
coeftest(dfit7)

########## 5. Make predictions

hold <- window(ts(cleaned_tsdata1), start =327)

fit_predicted <- arima(ts(cleaned_tsdata1[-c(327:351)]), order =c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
forecast_pred <- forecast(fit_predicted,h=24) 
plot(forecast_pred, main="")
lines(ts(my_ts))



#forecasting
f_values <-forecast(dfit7, h=24) 
plot(f_values, main="")


