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

#Autocorrelation and Partial Autocorrelation Plots
Acf(cleaned_tsdata)
Pacf(cleaned_tsdata)


########## 2. Stationarize the series

########## 3. Plot ACF/PACF charts and find optimal parameters

########## 4. Build the ARIMA model

########## 5. Make predictions





