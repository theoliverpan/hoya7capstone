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
head(tsdata)

#exploratory plot
ts_plot <- ggplot(auto, aes(Period,Value)) + geom_line(na.rm=TRUE) + 
  xlab("Month") + ylab("Auto Sales in Thousands") + 
  scale_x_date(labels = date_format(format= "%b-%Y"),breaks = date_breaks("1 year")) + 
  stat_smooth(colour = "green")

########## 2. Stationarize the series

########## 3. Plot ACF/PACF charts and find optimal parameters

########## 4. Build the ARIMA model

########## 5. Make predictions





