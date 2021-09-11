### Hoya 7 problem set 1 code



#data prep
library(estimatr)
library("mvtnorm")

set.seed(123)
beta1 <- 1.0
beta2 <- 1.5
nobs <- 1000
x1 <- rnorm(nobs, mean = 0, sd = 1)
lambda <- rnorm(nobs, mean = 0, sd = 1)
x2 <- x1+ lambda
eps <- rnorm(nobs, mean = 0, sd = 1)
y <-beta1*x1 + beta2*x2 + eps
df <- data.frame(y, x1, x2)



#Question 1
#Interpret the precision of the estimate and the R2 of the regression.

results.lm <- lm(y ~ x1, data = df)
summary(results.lm)


library(broom)
library("jtools")
library("huxtable")
aug.results.lm <- augment(results.lm)
head(aug.results.lm)
#the following code plots residuals error (red) between observed values and a fitted regression line

ggplot(aug.results.lm, aes(x1, y)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = x1, yend = .fitted), color = "red", size = 0.3)
## `geom_smooth()` using formula 'y ~ x'
# Produce a scatter plot with the value of x1 on the horizontal access and the value of the residuals on the vertical axis.
residuals_graph <- ggplot(df) +
  aes(x = x1, y = results.res) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  geom_smooth(method=lm) +
  labs(x = "x1", y = "Residuals", title = "Scatter Plot 1: Relationship between Regressor and Residuals") +
  theme_minimal()

residuals_graph
## `geom_smooth()` using formula 'y ~ x'
# Diagnostic Plots including residuals vs fitted, qq plot, scale location and residuals vs leverage

library("ggfortify")

par(mfrow = c(2,2))
plot(results.lm)

#Question 2
# The covariance of (x1, y) divided by the variance of x1 is 2.61. This value is identical to the regression estimate. 

cov(x1, y) / var(x1)
## [1] 2.613001

#Question 4
# Multivariate OLS Regression 
results.multi.lm <- lm(y ~ x1 + x2, data = df)
# Compare to univariate OLS regression
jtools::export_summs(results.lm, results.multi.lm)
library("ggstance") #leveraged to compare how the control affects the coefficient (how adding x1 affects the coefficient)
plot_coefs(results.lm, results.multi.lm)

#Question 5
#Estimate the univariate regression of y on x2
lm.ry <- lm(y ~ x2, data = df)
summary(lm.ry)
#Store residuals as ry

ry <- resid(lm.ry)

#Estimate the univariate regression of x1 on x2
lm.rx <- lm(x1 ~ x2, data = df)
summary(lm.rx)
#Store residuals as rx

rx <- resid(lm.rx)

# Estimate the univariate regression of ry on rx

lm.ry.rx <- lm(ry ~ rx, data = df)
summary(lm.ry.rx)
# table all estimates for point estimates and R squared
jtools::export_summs(lm.ry, lm.rx, lm.ry.rx)

#Question 6
OVB1 <- beta2*((cov(x1, x2)/var(x1)))
OVB1
## [1] 1.632071
# Univariate Estimate less Multivariate Estimate 
results.lm$coefficients[2] - results.multi.lm$coefficients[2]
##    x1 
## 1.662

#Question 9
library(AER)
part2_data <- read.csv("/Users/oliverpanbiz/Documents/Georgetown MSBA/Classes/Econ/Week 1 & 2/data/newdata123.csv")

q9model <- lm(part2_data$momweeksworked ~ part2_data$kidcount,
              data = part2_data)
summary(q9model)

#Question 13
library(tidyverse)
pt2 = pt2Data %>% 
  filter(momworked == "1")
ols13 = lm(log(pt2$momweeksworked) ~ pt2$kidcount ,data= pt2)
summary(ols13)
stage1 = lm(pt2$kidcount ~ pt2$samesex,data= pt2)
summary(stage1)
stage2 = lm(log(pt2$momweeksworked) ~ fitted.values(stage1),
            data = pt2)
summary(stage2)
sls13 <- ivreg(log(pt2$momweeksworked) ~ pt2$kidcount  |  pt2$samesex)
summary(sls13)

#Question 14
ols14 = lm(pt2$kidcount ~ pt2$twins2,data= pt2)
summary(ols14)
sls14 <- ivreg(log(pt2$momweeksworked) ~ pt2$kidcount   |  pt2$twins2)
summary(sls14)
sls14pt2 <- ivreg(log(pt2$momweeksworked) ~ pt2$kidcount   |  pt2$twins2 + pt2$samesex)
summary(sls14pt2)

#Question 16
results.multi.lm.1 <- lm(y ~ x1 + x2 + ry, data = df)

jtools::export_summs(results.multi.lm.1)
