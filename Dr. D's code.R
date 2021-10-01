#-------------------------------------#
#          MARK601: Week5             #  
#     TIME SERIES FORECASTING-1       #
#      Dr Sudipta Dasmohapatra        #
#-------------------------------------#

#ANALYZE TIME SERIES DATA
library(ggplot2)
library(forecast)
library(grid)
library(Amelia)
library(tseries)
library(scales)
library(gridExtra)
library(lmtest)

install.packages("Rcpp")
library(Rcpp)

#read the data
auto <- read.csv("C:/Users/.../dautonsa.final.csv",header =TRUE, stringsAsFactors = F)

summary(auto)

#1. Missing data check
sum(is.na(auto))
library(Amelia)
library(Rcpp)
missmap(auto,main = "Missing Values",col = c('light blue','Blue'),x.cex = 1.5)


#first convert the date field from character to date type
auto$DATE <- as.Date(auto$DATE, "%m/%d/%Y")
head(auto)

#2. Plot monthly sales data
ts_plot <- ggplot(auto, aes(DATE,DAUTONSA)) + geom_line(na.rm=TRUE) + 
  xlab("Month") + ylab("Auto Sales in Thousands") + 
  scale_x_date(labels = date_format(format= "%b-%Y"),breaks = date_breaks("1 year")) + 
  stat_smooth(colour = "green")

ts_plot
class(auto)
#[1] "data.frame"

#Converting data into a time series object
auto_ts <-ts(auto[,c('DAUTONSA')])
class(auto_ts)
#[1] "ts"

#Other Time Series Plots
#Plot time series with trend line
plot(auto_ts, col = "blue", main = "Auto Sales Time Series Data")
abline(reg=lm(auto_ts~time(auto_ts)), col="lightgray") #plotting the trend line

#Autocorrelation and Partial Autocorrelation Plots
Acf(auto_ts)
Pacf(auto_ts)

#Lag plot of Data
gglagplot(auto_ts, set.lags=1:16)

#The ACF plots test if an individual lag autocorrelation is different than zero. 
#An alternative approach is to use the Ljung-Box test, 
#which tests whether any of a group of autocorrelations of a time series are different from zero. 
#In essence it tests the “overall randomness” based on a number of lags.
#If the result is a small p-value than it indicates the data are probably not white noise.
#Ljung-Box test on the first 24 Lag autocorrelations

Box.test(auto_ts, lag=24, fitdf=0, type="Lj")

###DECOMPOSING THE TIME SERIES (additive)
#Converting data into a time series object by year
auto_tsd <-ts(auto[,c('DAUTONSA')], frequency=12)
class(auto_tsd)
component.ts = decompose(auto_tsd)
plot(component.ts)

#Or use the following
component.tsa = decompose(auto_tsd, type="additive", filter=NULL)
plot(component.tsa)

#######################################
##EXTRA CODE
#For multiplicative decomposition use the following code
component.tsm2 = decompose(auto_tsd, type="multiplicative", filter=NULL)
plot(component.tsm2)

#OR

component.tsm2 = decompose(log(auto_tsd))
plot(component.tsm2)

#If you are using multiplicative decomposition
#In all subsequent modeling purposes, use the log of the variable
######################################

#We can also use STL (Seasonal and Trend Decomposition using LOESS)
#LOESS is a method for estimating nonlinear relationships
#Advantage of LOESS: handle any type of seasonality, seasonal component changes over time, 
#smoothness of trend can be controlled by user and can be robust to outliers

auto_tsd %>%
  stl(t.window=24, s.window="periodic", robust=TRUE) %>%
  autoplot()

#tsclean() identifies and replaces outliers using series smoothing and decomposition.
#tsclean() can also impute missing values in the series if there are any
#We are using the ts() command to create a time series object to pass to tsclean()
auto$csales <-tsclean(auto_ts)

#Plot the cleaned data
c_ts_plot <- ggplot(auto, aes(DATE,csales)) + geom_line(na.rm=TRUE) + 
  xlab("Month") + ylab("Auto Sales in Thousands") + 
  scale_x_date(labels = date_format(format= "%b-%Y"),breaks = date_breaks("1 year")) + 
  stat_smooth(colour="green")
c_ts_plot

#Lets compare both cleaned and uncleaned plots
grid.arrange(ts_plot,c_ts_plot,ncol=1, top = textGrob("Uncleaned vs Cleaned Series"))

#Smoothing the time series and looking at the decomposed time series again
my_ts <- ts(na.omit(auto$csales), frequency = 12)
plot(my_ts)

component.ts2 = decompose(my_ts)
plot(component.ts2)

#3. Naive Forecasting Method (for the next 24 months after observed data)
naive_forecast <-naive(auto_ts, 24)
summary(naive_forecast)
autoplot(naive_forecast)

#Check for fitted values and residuals
checkresiduals(naive_forecast)

#4. Smoothing the Series to uncover patterns in data
#4.1 Moving Averages
#MA of order 5 (generally of odd numbers)

auto_ma<-ma(auto_ts, 5)
autoplot(auto_ts, series="Data") +
  autolayer(ma(auto_ts,5), series="5-MA") +
  xlab("Year") + ylab("Sales") +
  ggtitle("Auto Sales Moving Average - 5 months") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))

#MA of order 3
autoplot(auto_ts, series="Data") +
  autolayer(ma(auto_ts,3), series="3-MA") +
  xlab("Year") + ylab("Sales") +
  ggtitle("Auto Sales Moving Average - 3 months") +
  scale_colour_manual(values=c("Data"="grey50","3-MA"="red"),
                      breaks=c("Data","3-MA"))

#MA of order 9
autoplot(auto_ts, series="Data") +
  autolayer(ma(auto_ts,9), series="9-MA") +
  xlab("Year") + ylab("Sales") +
  ggtitle("Auto Sales Moving Average - 9 months") +
  scale_colour_manual(values=c("Data"="grey50","9-MA"="red"),
                      breaks=c("Data","9-MA"))

#Moving Average of Moving Averages (only for even order moving average to make them symmetric)
#A 2x4 moving average

autoplot(auto_ts, series = "Data") + 
  autolayer(ma(auto_ts, order = 4, centre = TRUE), series = "2x4-MA") +
  labs(x = "Year", y = "Sales") + 
  ggtitle("2x4 moving average of autosales")

#Removing Seasonal effects (if it is there- say a 1 year seasonal variation)
autoplot(auto_ts, series = "Data") + 
  autolayer(ma(auto_ts, 12), series = "12-MA") +
  labs(x = "Year", y = "Sales") + 
  ggtitle("12-month moving average of autosales") +
  scale_colour_manual(values=c("Data"="grey50","12-MA"="red"),
                      breaks=c("Data","12-MA"))

#4.2 Exponential Smoothing Models
#Simple exponential smoothing used only for models that dont have any trend or Seasonality
auto_ses <-ses(auto_ts, alpha=0.2, h=24)
autoplot(auto_ses)

#We can remove the trend simply by differencing the data
auto_dif <-diff(auto_ts)
autoplot(auto_dif)

#Once we’ve differenced we’ve effectively removed the trend from our data and can reapply the SES model
auto_ses2 <-ses(auto_dif, alpha=0.2, h=24)
autoplot(auto_ses2)

#5. Making the series stationary (identify level of differencing required) 
#we need to remove trend by using appropriate order of difference and make the series stationary. 
#We do this by looking at acf, Dickey-Fuller Test and standard deviation.
#DICKEY FULLER TEST 
#(We have to test if Rho - 1 is significantly different than zero or not. 
#If the null hypothesis gets rejected, we’ll get a stationary time series.)
#First, confirm that the series is non-stationary using augmented DF test
adf.test(my_ts)

#To convert series to stationary, we need to know the level of differencing required
#Look at ACF (autocorrelation plot for the series to identify the order of differencing required)
Acf(my_ts)
Pacf(my_ts)

#6. Forecasting with ARIMA Model
#using differencing: lets try order 1 difference
#We will fit ARIMA(0,d,0)(0,D,0)[12] models 
#and verify acf residuals to find which ‘d’ or ‘D’ order of differencing is appropriate in our case.
#Applying only one order of difference i.e ARIMA(0,1,0)(0,0,0)
dfit1 <-arima(my_ts, order=c(0,1,0))
plot(residuals(dfit1))

Acf(residuals(dfit1))
Pacf(residuals(dfit1))

#Because the seasonal pattern is strong and stable, 
#we will want to use an order of seasonal differencing in the model. 
#Before that let’s try only with one seasonal difference i.e ARIMA(0,0,0)(0,1,0)

dfit2 <- arima(my_ts, order =c(0,0,0), seasonal = list(order = c(0,1,0), period = 12))
plot(residuals(dfit2))
Acf(residuals(dfit2))
Pacf(residuals(dfit2))

#lets try and apply both seasonal and non-seasonal differencing, ARIMA(0,1,0)(0,1,0)[12]
dfit3 <- arima(my_ts, order =c(0,1,0), seasonal = list(order = c(0,1,0), period = 12))
plot(residuals(dfit3))
Acf(residuals(dfit3))
Pacf(residuals(dfit3))

#Since first ACF is -ve and most of the positive correlations are now negative (series is overdifferenced)
#we should add an MA term to the model but to know what order of MA we need,
#check the standard deviation of the models (sd=RMSE) 
summary(dfit1)
summary(dfit2)
summary(dfit3)

#We have over-differencing, so we will stop here, 
#Out of the above, dfit3 model, i.e., ARIMA(0,1,0)(0,1,0)12 has the lowest standard deviation(RMSE) and AIC. 
#Therefore, it is the correct order of differencing.
#Now, we need to identify AR/MA and SAR/SMA values and fit the model

dfit4 <- arima(my_ts, order =c(0,1,1), seasonal = list(order = c(0,1,0), period = 12))
plot(residuals(dfit4))
Acf(residuals(dfit4))
Pacf(residuals(dfit4))

#Add a one-order MA component to the seasonal part and see what we get
dfit5 <- arima(my_ts, order =c(0,1,0), seasonal = list(order = c(0,1,1), period = 12))
plot(residuals(dfit5))
Acf(residuals(dfit5))
Pacf(residuals(dfit5))

#combine a MA component to non-seasonal and one to seasonal
dfit6 <- arima(my_ts, order =c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
plot(residuals(dfit6))
Acf(residuals(dfit6))
Pacf(residuals(dfit6))

#Pending statistically significant MA coefficient and low AIC the model seems a good fit
summary(dfit4)
summary(dfit5)
summary(dfit6)

#The coeftest() function in lmtest package can help us in getting the p-values of coefficients.
coeftest(dfit6)

#Check Minimum AIC and Iterate
#We use the auto.arima() function to let R build our model with least AIC
#this function will search through combination of order parameters and provide best set
#by default it looks at maximum order of size 5 
dfit7 <- auto.arima(my_ts, seasonal = TRUE)
plot(residuals(dfit7))
Acf(residuals(dfit7))
Pacf(residuals(dfit7))

summary(dfit7)
coeftest(dfit7)

#7. Model Validation (n-fold holdout method)
hold <- window(ts(my_ts), start =233)

#we will forecast data for the last two years (month = 233 to 256)
fit_predicted <- arima(ts(my_ts[-c(233:256)]), order =c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))

#use the model to forecast values for last 24 months. 
#Specify forecast horizon h periods ahead of prediction to be made 
#and use the fitted model to generate those predictions

forecast_pred <- forecast(fit_predicted,h=24)
plot(forecast_pred, main="")
lines(ts(my_ts))

#8. Forecasting
#Next step is to forecast the sales for another 24 months ahead of time. 
f_values <-forecast(dfit6, h=24)
plot(f_values, main="")

############################################################
#1. To improve the forecast, take only the most recent data from 2011 onwards
#2. Add covariates into the model
############################################################




