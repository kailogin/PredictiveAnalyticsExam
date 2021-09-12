# Packages
library(fpp2)
library(AER)
library("readxl")
library(urca)
library(gridExtra)
library(strucchange)
library(forecast)
library(tseries)
library(ggplot2)

#Load Dataset
#Column 2 -> Real GDP in GER from 1950 until 2019
my_data <- read_excel("/Users/Kai/Desktop/Predictive_Final/GER_RealGDP.xls")
print(my_data)

gdp_ts <- ts(my_data[,2], start = c(1950), end= c(2019))
gdp_ts
autoplot(gdp_ts)

autoplot(gdp_ts) + ylab("Billion USD") +
  ggtitle("Germany: Real GDP in Billion USD")

# ACF and PACF charts 

grid.arrange((ggAcf(gdp_ts)
              + ylab("")
              + ggtitle("ACF")),
             (ggPacf(gdp_ts)
              + ylab("")
              + ggtitle("PACF")),
             nrow = 1)

# Go with Log-Transformation of TS
# gdp_ts_log <- log(gdp_ts)
# autoplot(gdp_ts_log)
# grid.arrange((ggAcf(gdp_ts_log)
#               + ylab("")
#               + ggtitle("ACF")),
#              (ggPacf(gdp_ts_log)
#               + ylab("")
#               + ggtitle("PACF")),
#              nrow = 1)
# 
# grid.arrange((ggAcf(diff(diff(gdp_ts_log)))
#               + ylab("")
#               + ggtitle("ACF")),
#              (ggPacf(diff(gdp_ts_log))
#               + ylab("")
#               + ggtitle("PACF")),
#              nrow = 1)


## Check for Stationary Data

# Number of Differences
ndiffs(gdp_ts, test = "adf")
# 1
ndiffs(gdp_ts, test = "kpss")
# 1

# # Difference to make data stationary

ts_diff <- diff(gdp_ts)

summary(ur.kpss(gdp_ts, type='tau'))


# Dickey-Fuller Test
summary(ur.df(gdp_ts, type = 'trend', selectlags = 'AIC'))


#Confirm with drift and mu tests that the time series has a drift
summary(ur.kpss(gdp_ts, type='mu'))

summary(ur.df(gdp_ts, type = 'drift', selectlags = 'AIC'))

# PACF and ACF from stationary data
grid.arrange((ggAcf(ts_diff)
              + ylab("")
              + ggtitle("ACF")),
             (ggPacf(ts_diff)
              + ylab("")
              + ggtitle("PACF")),
             nrow = 1)

# Plot the differenced data
autoplot(ts_diff) +
  ggtitle("Growth Rate Real GDP in Germany")

##### Check again for Stationarity after Differencing

summary(ur.kpss(ts_diff, type='mu'))

summary(ur.df(ts_diff, type = 'drift', selectlags = 'AIC'))
# ----------------------------------------------------------------------------------------------------------

### STRUCTURAL BREAKS 
library(strucchange)

d <- ts.intersect(y = ts_diff, y1 = stats::lag(ts_diff), -1)
fs <- Fstats(y ~ y1, data = d)
plot(fs)

ts_ols <- Fstats(y ~ y1, data = d, from = 0.15)
ts_ols_test <- sctest(ts_ols, type = "supF")
ts_ols_test

# ----------------------------------------------------------------------------------------------------------

# Check ACF and PACF for Differenced Data
grid.arrange((ggAcf(ts_diff)
              + ylab("")
              + ggtitle("ACF")),
             (ggPacf(ts_diff)
              + ylab("")
              + ggtitle("PACF")),
             nrow = 1)

# ARIMA
# ---------------------------------------------------------------------------------------------------------
# #Use auto.arima to confirm the results

# OWN ARIMA MODEL
own_arima_model <- Arima(gdp_ts,order=c(1,1,0))

# ACTUAL MODEL
model_auto_arima <- auto.arima(gdp_ts, ic="aic")
model_auto_arima
checkresiduals(model_auto_arima)


# ETS
# ---------------------------------------------------------------------------------------------------------


# ETS OWN MODEL SELECTION
model_ETS_OWN <- ets(gdp_ts, model = "MMN")
summary(model_ETS_OWN)
checkresiduals(model_ETS_OWN)

# ETS RANDOM MODEL SELECTION

model_ETS_RANDOM <- ets(gdp_ts)
summary(model_ETS_RANDOM)
checkresiduals(model_ETS_RANDOM)

# ETS RANDOM with damped trend
model_ETS_RANDOM_damped = ets(gdp_ts, damped = TRUE)
summary(model_ETS_RANDOM_damped)


# CROSS-VALIDATION
# ---------------------------------------------------------------------------------------------------------

# #Split data into train and test dataset - 80/20

# For the differenced data
gdp_diff.train <- window(ts_diff, end=c(2006,1))
autoplot(gdp_diff.train)
gdp_diff.test <- window(ts_diff, start=c(2007,1))
autoplot(gdp_diff.test)


# For the undifferenced data
gdp_train <- window(gdp_ts, end=c(2006,1))
autoplot(gdp_train)
gdp_test <- window(gdp_ts, start=c(2007,1))
autoplot(gdp_test)

h <- length(gdp_test)
h

autoplot(gdp_train)+
  autolayer(gdp_test, PI = FALSE, series="ARIMA(2,1,0)")

# FORECASTING 
# ---------------------------------------------------------------------------------------------------------

# ##### ETS

# MAN
ets_train <- ets(gdp_train, model = "MAN")
fc.ets <- forecast(ets_train, h=h)
autoplot(fc.ets)

# MAdN
ets_train_damped <- ets(gdp_train, model = "MAN", damped=TRUE)
fc_ets_train_damped <- forecast(ets_train_damped, h=h)

# MMN
ets_train_MMN <- ets(gdp_train, model = "MMN")
fc_ets_MMN <- forecast(ets_train_MMN, h=h)

####### ARIMA

# AUTO ARIMA
arima.fit <- auto.arima(gdp_train, ic="aic")
fc.arima <- forecast(arima.fit, h=h)

# OWN ARIMA
own_ari <- Arima(gdp_train,order=c(1,1,0))
fc.own_arima <- forecast(own_ari, h=h)
autoplot(fc.own)

# PLOT TWO MOST SUCCESSFUL FORECASTS
autoplot(gdp_ts)+
  autolayer(fc.arima, PI = FALSE, series="ARIMA(2,1,0)")+
  autolayer(fc.ets, PI = FALSE, series="ETS(M,A,N)") +
  ggtitle("13 year forecasts for ARIMA and ETS model") +
  ylab("")

# PLOT ALL FORECASTS TOGETHER
autoplot(gdp_ts)+
  autolayer(fc.arima, PI = FALSE, series="ARIMA(2,1,0)")+
  autolayer(fc.own_arima, PI = FALSE, series="ARIMA(1,1,0)")+
  autolayer(fc.ets, PI = FALSE, series="ETS(M,A,N)") +
  autolayer(fc_ets_MMN, PI = FALSE, series="ETS(M,M,N)") +
  autolayer(fc_ets_train_damped, PI = FALSE, series="ETS(M,Ad,N)") +
  ggtitle("13 year forecasts for ARIMA and ETS models") +
  ylab("")

# ARIMA INTERVAL
autoplot(fc.arima) +
  ylab("") +
  autolayer(gdp_test, col=3, series="Test Dataset") +
  ggtitle("Forecast 13 Years ARIMA(2,1,0)")

# autoplot(fc.own_arima) +
#   ylab("") +
#   autolayer(gdp_test, col=3, series="Test set") +
#   ggtitle("Forecast 13 Years ARIMA(1,1,0)")

# ETS INTERVAL

autoplot(fc.ets) +
  ylab("") +
  autolayer(gdp_test, col=3, series="Test Dataset") +
  ggtitle("Forecast 13 Years ETS(M,A,N)")

# autoplot(fc_ets_MMN) +
#   ylab("") +
#   autolayer(gdp_test, col=3, series="Test set") +
#   ggtitle("Forecast 13 Years ETS(M,M,N)")
# 
# autoplot(fc_ets_train_damped) +
#   ylab("") +
#   autolayer(gdp_test, col=3, series="Test set") +
#   ggtitle("Forecast 13 Years ETS(M,Ad,N)")


# Accuracy measures
accuracy(fc.arima, gdp_test)
accuracy(fc.own_arima, gdp_test)
accuracy(fc.ets, gdp_test)
accuracy(fc_ets_MMN, gdp_test)
accuracy(fc_ets_train_damped, gdp_test)


