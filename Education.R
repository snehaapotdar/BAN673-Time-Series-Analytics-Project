library(forecast)

# set working directory for locating files.
setwd("C:/Users/saurabhn/Desktop/Snehaa/Time_Series_Analytics/Project/Project_Files_Collab/reban673timeseriesanalyticsgroupproject")

#setwd("E:/MS/Time Series Analytics/project")
#setwd("G:/CSUEB/MSBA/Spring 19/BAN_673_Forecasting/Project/")
# Create data frame.
nz.data <- read.csv("Processed_Raw_Dataset_NZ.csv")

education.ts <- ts(nz.data$Education, start = c(2000, 1), end = c(2012, 10), freq = 12)
# ----------<<<<EDUCATION DATASET>>>>-------------
summary(education.ts)
## Use plot() to plot time series data  
plot(education.ts, 
     xlab = "Time", ylab = "Student Arrivals", 
     ylim = c(3000, 20000), main = "Student Travelers By Time Period", col = "blue")

# Create fixed data partitioining for data.
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 22
nTrain <- length(education.ts) - nValid
train_education.ts <- window(education.ts, start = c(2000, 1), end = c(2000, nTrain))
valid_education.ts <- window(education.ts, start = c(2000, nTrain + 1), 
                             end = c(2000, nTrain + nValid))

# Create Holt-Winter's exponenthial smoothing (HW).
# Use ets() function with model = "ZZZ", i.e., automated selection 
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw_education.ZZZ <- ets(train_education.ts, model = "ZZZ") 
hw_education.ZZZ # Model appears to be (A, Ad, A), with alpha = 0.9999, beta=0.087,gamma=1e-04,phi=0.9359

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw_education.ZZZ.pred <- forecast(hw_education.ZZZ, h = nValid, level = 0)
hw_education.ZZZ.pred

round(accuracy(hw_education.ZZZ.pred, valid_education.ts), 3)
#                   ME     RMSE     MAE   MPE  MAPE  MASE  ACF1 Theil's U
# Training set  10.558  404.213 302.363 0.193 2.862 0.181 0.045        NA
# Test set     626.885 1094.675 913.645 4.233 6.512 0.546 0.738      0.67

plot(hw_education.ZZZ.pred, 
     xlab = "Time", ylab = "Student Arrivals", ylim = c(3000, 20000), bty = "l",
     xaxt = "n", xlim = c(2000, 2013), 
     main = "Holt-Winter's Model for Student Travelers with Automated Selection \n of Model Options", flty = 2) 
axis(1, at = seq(2000, 2013, 1), labels = format(seq(2000, 2013, 1)))
lines(hw_education.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(valid_education.ts)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(3000, 20000))
lines(c(2011, 2011), c(3000, 20000))
text(2005.5, 19000, "Training")
text(2012, 19000, "Validation")

arrows(2010.75, 18000, 2000.25, 18000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2011.25, 18000, 2012.75, 18000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2002,8000, legend = c("Student Arrival", "Student Arrival HW's (A Ad A) model",
                             "HW's forecast for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")


#------------------Regression Model with Quadratic Trend & Seasonality-------------------
# Use tslm() function to create quadratic trend and seasonal model.
train.trend.season <- tslm(train_education.ts ~ trend + I(trend^2) + season)
summary(train.trend.season)#season 12 coeff has insignificant p-value
# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)
train.trend.season.pred
round(accuracy(train.trend.season.pred, valid_education.ts),3)
#                    ME     RMSE      MAE    MPE   MAPE  MASE  ACF1 Theil's U
# Training set    0.000 1811.038 1392.467 -2.884 12.500 0.832 0.970        NA
# Test set     1468.558 1643.977 1485.700 10.516 10.658 0.888 0.649     1.022

#-------------------ACF on residuals data-----------------
Acf(train.trend.season$residuals, lag.max = 12, 
    main = "Autocorrelation for Student Arrivals Training Residuals")
# Acf(train.trend.season.pred$residuals, lag.max = 12, 
#     main = "Autocorrelation for Arrivals pred Residuals")
Acf(valid_education.ts - train.trend.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Student Validation Residuals")

res.ar1 <- Arima(train.trend.season$residuals, order = c(1,0,0))
summary(res.ar1)
# Coefficients:
#   ar1       mean
# 0.9727  -283.5201
# s.e.  0.0174  1047.7501
?Arima
# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

valid.two.level.pred <- train.trend.season.pred$mean + res.ar1.pred$mean

round(accuracy(valid.two.level.pred, valid_education.ts), 3)
#                ME    RMSE      MAE   MPE  MAPE  ACF1 Theil's U
# Test set 1268.696 1442.59 1292.425 9.057 9.254 0.602       0.9

plot(train.trend.season.pred$fitted + res.ar1.pred$fitted, 
     xlab = "Time", ylab = "Student Arrivals", ylim = c(3000, 20000), bty = "l",
     xaxt = "n", xlim = c(2000, 2013), col = "blue",
     main = "Combined two-level Model for Student Arrivals") 
axis(1, at = seq(2000, 2013, 1), labels = format(seq(2000, 2013, 1)))
lines(valid.two.level.pred, col = "blue", lwd = 2, lty =4)
lines(train_education.ts, col = "black", lwd = 1, lty = 1)
lines(valid_education.ts, col = "black", lwd = 1, lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(3000, 20000))
lines(c(2011, 2011), c(3000, 20000))
text(2005.5, 19000, "Training")
text(2012, 19000, "Validation")

arrows(2010.75, 18000, 2000.25, 18000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2011.25, 18000, 2012.75, 18000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2002,8000, legend = c("Student Arrival", "Student Arrival Two-level model",
                             "Two-level forecast for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")


#-------------------AUTO ARIMA-------------------
# Use auto.arima() function to fit ARIMA model.
# use summary() to show auto ARIMA model and its parameters for entire dataset.
train_education.auto.arima <- auto.arima(train_education.ts)
summary(train_education.auto.arima)
# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train_education.auto.arima.pred <- forecast(train_education.auto.arima, h = nValid, level = 0)
summary(train_education.auto.arima.pred)

round(accuracy(train_education.auto.arima.pred, valid_education.ts), 3)
#                   ME    RMSE     MAE    MPE  MAPE  MASE   ACF1 Theil's U
# Training set -31.356 402.248 285.089 -0.230 2.227 0.170 -0.009        NA
# Test set     451.529 974.181 804.595  2.964 5.747 0.481  0.729     0.598

plot(train_education.auto.arima.pred, 
     xlab = "Time", ylab = "Student Arrivals", ylim = c(3000, 20000), bty = "l",
     xaxt = "n", xlim = c(2000, 2013), 
     main = "Auto ARIMA Model for Student Arrivals", flty = 2) 
axis(1, at = seq(2000, 2013, 1), labels = format(seq(2000, 2013, 1)))
lines(train_education.auto.arima$fitted, col = "blue", lwd = 2)
lines(valid_education.ts)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(3000, 20000))
lines(c(2011, 2011), c(3000, 20000))
text(2005.5, 19000, "Training")
text(2012, 19000, "Validation")

arrows(2010.75, 18000, 2000.25, 18000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2011.25, 18000, 2012.75, 18000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2002,8000, legend = c("Student Arrival", "Student Arrival Auto-ARIMA model",
                             "Auto-ARIMA forecast for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#####################################################################
##################---------ENTIRE DATA SET--------###################
#####################################################################
#-----------------------------HW model-----------------------------------------
#HW model on full data set with ZZZ
education_HW.ZZZ <- ets(education.ts, model = "ZZZ") 
summary(education_HW.ZZZ)
# ETS(A,Ad,A) 
# 
# Call:
#   ets(y = education.ts, model = "ZZZ") 
# 
# Smoothing parameters:
#   alpha = 0.9999 
# beta  = 0.0782 
# gamma = 1e-04 
# phi   = 0.939 
# 
# Initial states:
#   l = 5217.7949 
# b = 295.7373 
# s = -2332.93 125.499 560.9344 421.7179 1368.878 1665.675
# -216.7435 -100.3409 421.7643 923.7542 93.5823 -2931.791
# 
# sigma:  438.2762
# 
# AIC     AICc      BIC 
# 2667.195 2672.261 2721.860 
# 
# Training set error measures:
#   ME     RMSE      MAE        MPE     MAPE      MASE       ACF1
# Training set -0.6974673 413.3784 310.2805 0.08015016 2.842661 0.1976956 0.05533232


#forecast for Nov 2012 to Dec 2014 using HW model
education_HW.ZZZ.pred <- forecast(education_HW.ZZZ, h = 14, level = c(0, 95))
education_HW.ZZZ.pred

round(accuracy(education_HW.ZZZ.pred$fitted, education.ts), 3)
#              ME    RMSE     MAE  MPE  MAPE  ACF1 Theil's U
# Test set -0.697 413.378 310.281 0.08 2.843 0.055      0.37

plot(education_HW.ZZZ.pred, 
     xlab = "Time", ylab = "Student Arrivals", ylim = c(3000, 20000), bty = "l",
     xaxt = "n", xlim = c(2000, 2014), 
     main = "Holt-Winter's Model Forecast for Students with Automated Selection", flty = 2) 
axis(1, at = seq(2000, 2014, 1), labels = format(seq(2000, 2014, 1)))
lines(education_HW.ZZZ.pred$fitted, col = "blue", lwd = 2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(3000, 20000))
lines(c(2012.83, 2012.83), c(3000, 20000))
text(2005.5, 19000, "Historical data")
text(2013.5, 19000, "Forecast")

arrows(2012.75, 18000, 2000.25, 18000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2012.9, 18000, 2013.9, 18000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2006,8000, legend = c("Student Arrival", "Student Arrival HW's (A,Ad,A) model",
                               "HW's forecast"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#-----------------------------Auto-ARIMA model------------------------------------
# Use auto.arima() function to fit ARIMA model for entire set
# use summary() to show auto ARIMA model and its parameters for entire dataset.
education.auto.arima <- auto.arima(education.ts)
summary(education.auto.arima)
# ARIMA(0,1,1)(0,1,1)[12] 
# 
# Coefficients:
#   ma1     sma1
# 0.1769  -0.6040
# s.e.  0.0847   0.0975
# 
# sigma^2 estimated as 199309:  log likelihood=-1062.09
# AIC=2130.17   AICc=2130.35   BIC=2139.02
# 
# Training set error measures:
#   ME     RMSE      MAE        MPE     MAPE      MASE        ACF1
# Training set -51.43758 424.1415 307.1318 -0.3942933 2.362397 0.1956894 -0.01334622

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
education.auto.arima.pred <- forecast(education.auto.arima, h = 14, level = c(0,95))
education.auto.arima.pred

round(accuracy(education.auto.arima.pred$fitted,education.ts),3)
#               ME    RMSE     MAE    MPE  MAPE   ACF1 Theil's U
# Test set -51.438 424.141 307.132 -0.394 2.362 -0.013     0.248

plot(education.auto.arima.pred, 
     xlab = "Time", ylab = "Education Arrivals", ylim = c(3000, 20000), bty = "l",
     xaxt = "n", xlim = c(2000, 2014), 
     main = "Auto ARIMA Model for Education", flty = 2) 
axis(1, at = seq(2000, 2014, 1), labels = format(seq(2000, 2014, 1)))
lines(education.auto.arima.pred$fitted, col = "blue", lwd = 2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(3000, 20000))
lines(c(2012.83, 2012.83), c(3000, 20000))
text(2005.5, 19000, "Historical data")
text(2013.5, 19000, "Forecast")

arrows(2012.75, 18000, 2000.25, 18000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2012.9, 18000, 2013.9, 18000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2006,8000, legend = c("Education Arrival", "Auto ARIMA(0,1,1)(0,1,1)[12] model",
                               "Auto ARIMA forecast"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#snaive model
round(accuracy((snaive(education.ts))$fitted, education.ts), 3)
#Auto-ARIMA
round(accuracy(education.auto.arima.pred$fitted, education.ts),3)
#HW's Auto-fitted model
round(accuracy(education_HW.ZZZ.pred$fitted,education.ts),3)


#------------------Comparing Validation performance
round(accuracy(snaive(train_education.ts), valid_education.ts), 3)
round(accuracy(hw_education.ZZZ.pred, valid_education.ts), 3)
round(accuracy(train.trend.season.pred, valid_education.ts),3)
round(accuracy(valid.two.level.pred, valid_education.ts), 3)
round(accuracy(train_education.auto.arima.pred, valid_education.ts), 3)

