library(forecast)

# set working directory for locating files.
setwd("C:/Users/saurabhn/Desktop/Snehaa/Time_Series_Analytics/Project/Project_Files_Collab/reban673timeseriesanalyticsgroupproject")

#setwd("E:/MS/Time Series Analytics/project")
#setwd("G:/CSUEB/MSBA/Spring 19/BAN_673_Forecasting/Project/")
# Create data frame.
nz.data <- read.csv("Processed_Raw_Dataset_NZ.csv")

vacation.ts <- ts(nz.data$Vacation, start = c(2000, 1), end = c(2012, 10), freq = 12)


# ----------<<<<VACATION DATASET>>>>-------------

## Use plot() to plot time series data  
plot(vacation.ts, 
     xlab = "Time", ylab = "Vacation Arrivals", 
     ylim = c(30000, 220000), main = "Vacation Travelers By Time Period", col = "blue")

# Create fixed data partitioining for data.
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 22
nTrain <- length(vacation.ts) - nValid
train_vacation.ts <- window(vacation.ts, start = c(2000, 1), end = c(2000, nTrain))
valid_vacation.ts <- window(vacation.ts, start = c(2000, nTrain + 1), 
                            end = c(2000, nTrain + nValid))

# Create Holt-Winter's exponenthial smoothing (HW) for Amtrak data.
# Use ets() function with model = "ZZZ", i.e., automated selection 
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw_vacation.ZZZ <- ets(train_vacation.ts, model = "ZZZ") 
hw_vacation.ZZZ # Model appears to be (M, Ad, M), with alpha = 0.5865, beta=7e-04,gamma=4e-04,phi=0.98

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw_vacation.ZZZ.pred <- forecast(hw_vacation.ZZZ, h = nValid, level = 0)
hw_vacation.ZZZ.pred

round(accuracy(hw_vacation.ZZZ.pred, valid_vacation.ts), 3)
#                    ME     RMSE      MAE    MPE  MAPE  MASE  ACF1 Theil's U
# Training set  -45.524 3096.144 2300.831 -0.144 2.221 0.471 0.134        NA
# Test set     -872.146 7304.433 5819.922 -1.272 5.352 1.191 0.279     0.356


plot(hw_vacation.ZZZ.pred, 
     xlab = "Time", ylab = "Arrivals", ylim = c(30000, 230000), bty = "l",
     xaxt = "n", xlim = c(2000, 2013), 
     main = "Holt-Winter's Model for Vacation Travelers with Automated Selection of Model Options", flty = 2) 
axis(1, at = seq(2000, 2013, 1), labels = format(seq(2000, 2013, 1)))
lines(hw_vacation.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(valid_vacation.ts)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(30000, 220000))
lines(c(2011, 2011), c(30000, 220000))
text(2005.5, 230000, "Training")
text(2012, 230000, "Validation")

arrows(2010.75, 200000, 2000.25, 200000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2011.25, 200000, 2012.75, 200000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2004.5,70000, legend = c("Vacation Arrival", "Vacation Arrival HW's (M Ad M) model",
                              "HW's forecast for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")


#------------------Regression Model with Quadratic Trend & Seasonality-------------------
# Use tslm() function to create quadratic trend and seasonal model.
train.trend.season <- tslm(train_vacation.ts ~ trend + I(trend^2) + season)
summary(train.trend.season)#all coeff significant
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.431e+05  2.126e+03  67.336  < 2e-16 ***
#   trend        6.908e+02  5.059e+01  13.656  < 2e-16 ***
#   I(trend^2)  -3.342e+00  3.684e-01  -9.074 3.09e-15 ***
#   season2     -7.481e+03  2.343e+03  -3.193  0.00181 ** 
#   season3     -3.121e+04  2.343e+03 -13.319  < 2e-16 ***
#   season4     -6.200e+04  2.344e+03 -26.456  < 2e-16 ***
#   season5     -9.571e+04  2.344e+03 -40.836  < 2e-16 ***
#   season6     -1.052e+05  2.344e+03 -44.875  < 2e-16 ***
#   season7     -9.126e+04  2.344e+03 -38.925  < 2e-16 ***
#   season8     -9.489e+04  2.345e+03 -40.466  < 2e-16 ***
#   season9     -9.936e+04  2.345e+03 -42.366  < 2e-16 ***
#   season10    -9.027e+04  2.346e+03 -38.477  < 2e-16 ***
#   season11    -6.639e+04  2.347e+03 -28.293  < 2e-16 ***
#   season12    -1.628e+04  2.347e+03  -6.935 2.32e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5495 on 118 degrees of freedom
# Multiple R-squared:  0.9822,	Adjusted R-squared:  0.9803 
# F-statistic: 502.2 on 13 and 118 DF,  p-value: < 2.2e-16

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)
train.trend.season.pred
round(accuracy(train.trend.season.pred, valid_vacation.ts),3)
#                    ME     RMSE      MAE   MPE  MAPE  MASE  ACF1 Theil's U
# Training set    0.000 5195.742 4030.397 0.015 4.190 0.825 0.659        NA
# Test set     5475.752 9387.283 6156.363 4.187 4.977 1.260 0.308     0.404

#-------------------ACF on residuals data-----------------
Acf(train.trend.season$residuals, lag.max = 12, 
    main = "Autocorrelation for Vacation Arrivals Training Residuals")
# Acf(train.trend.season.pred$residuals, lag.max = 12, 
#     main = "Autocorrelation for Arrivals pred Residuals")
Acf(valid_vacation.ts - train.trend.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Vacation Validation Residuals")

res.ar1 <- Arima(train.trend.season$residuals, order = c(1,0,0))
summary(res.ar1)
# ARIMA(1,0,0) with non-zero mean 
# 
# Coefficients:
#   ar1       mean
# 0.7365  -175.5921
# s.e.  0.0650  1202.3995
# 
# sigma^2 estimated as 13988217:  log likelihood=-1272.63
# AIC=2551.26   AICc=2551.45   BIC=2559.91
# 
# Training set error measures:
#   ME    RMSE      MAE       MPE     MAPE      MASE       ACF1
# Training set 142.4329 3711.64 2675.669 -270.2704 410.8794 0.7354291 0.09333758

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

valid.two.level.pred <- train.trend.season.pred$mean + res.ar1.pred$mean

round(accuracy(valid.two.level.pred, valid_vacation.ts), 3)
#                ME     RMSE      MAE   MPE  MAPE  ACF1 Theil's U
# Test set 4452.678 8765.562 5887.693 3.416 5.004 0.359     0.402

plot(train.trend.season.pred$fitted + res.ar1.pred$fitted, 
     xlab = "Time", ylab = "Vacation Arrivals", ylim = c(30000, 230000), bty = "l",
     xaxt = "n", xlim = c(2000, 2013), col = "blue",
     main = "Combined two-level Model for Vacation Arrivals") 
axis(1, at = seq(2000, 2013, 1), labels = format(seq(2000, 2013, 1)))
lines(valid.two.level.pred, col = "blue", lwd = 2, lty =4)
lines(train_vacation.ts, col = "black", lwd = 1, lty = 1)
lines(valid_vacation.ts, col = "black", lwd = 1, lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(30000, 230000))
lines(c(2011, 2011), c(30000, 230000))
text(2005.5, 230000, "Training")
text(2012, 230000, "Validation")

arrows(2010.75, 200000, 2000.25, 200000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2011.25, 200000, 2012.75, 200000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2005,70000, legend = c("Vacation Arrival", "Vacation Arrival Two-level model",
                             "Two-level forecast for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#-------------------AUTO ARIMA-------------------
# Use auto.arima() function to fit ARIMA model.
# use summary() to show auto ARIMA model and its parameters for entire dataset.
train_vacation.auto.arima <- auto.arima(train_vacation.ts)
summary(train_vacation.auto.arima)
# ARIMA(1,1,1)(0,1,1)[12] 
# 
# Coefficients:
#   ar1      ma1     sma1
# 0.6284  -0.9073  -0.3979
# s.e.  0.0982   0.0499   0.0950
# 
# sigma^2 estimated as 14863914:  log likelihood=-1151.38
# AIC=2310.76   AICc=2311.11   BIC=2321.88

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train_vacation.auto.arima.pred <- forecast(train_vacation.auto.arima, h = nValid, level = 0)
summary(train_vacation.auto.arima.pred)

round(accuracy(train_vacation.auto.arima.pred, valid_vacation.ts), 3)
#                    ME     RMSE      MAE    MPE  MAPE  MASE  ACF1 Theil's U
# Training set -605.083 3614.170 2560.656 -0.927 2.493 0.524 0.005        NA
# Test set     -569.439 6843.139 5066.956 -0.843 4.773 1.037 0.436     0.343

plot(train_vacation.auto.arima.pred, 
     xlab = "Time", ylab = "Arrivals", ylim = c(30000, 230000), bty = "l",
     xaxt = "n", xlim = c(2000, 2013), 
     main = "Auto Arima Model for Vacation Travelers with Automated Selection of Model Options", flty = 2) 
axis(1, at = seq(2000, 2013, 1), labels = format(seq(2000, 2013, 1)))
lines(train_vacation.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid_vacation.ts)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(30000, 220000))
lines(c(2011, 2011), c(30000, 220000))
text(2005.5, 230000, "Training")
text(2012, 230000, "Validation")

arrows(2010.75, 200000, 2000.25, 200000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2011.25, 200000, 2012.75, 200000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2004.5,70000, legend = c("Vacation Arrivals", "Vacation Arrivals Auto Arima model",
                                "Auto Arima's forecast for Validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#------------------Comparing Validation performance
round(accuracy(snaive(train_vacation.ts), valid_vacation.ts), 3)
round(accuracy(hw_vacation.ZZZ.pred, valid_vacation.ts), 3)
round(accuracy(train.trend.season.pred, valid_vacation.ts),3)
round(accuracy(valid.two.level.pred, valid_vacation.ts), 3)
round(accuracy(train_vacation.auto.arima.pred, valid_vacation.ts), 3)


#####################################################################
##################---------ENTIRE DATA SET--------###################
#####################################################################
#--------------------------Two-level model----------------------------------------
##---Regression model with Quadratic trend and Seasonality for full data
trend.season <- tslm(vacation.ts ~ trend + I(trend^2) + season)
summary(trend.season)#all coeff significant
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.465e+05  2.116e+03  69.237  < 2e-16 ***
#   trend        6.297e+02  4.334e+01  14.531  < 2e-16 ***
#   I(trend^2)  -2.782e+00  2.708e-01 -10.271  < 2e-16 ***
#   season2     -9.609e+03  2.325e+03  -4.132 6.15e-05 ***
#   season3     -3.446e+04  2.325e+03 -14.817  < 2e-16 ***
#   season4     -6.441e+04  2.326e+03 -27.696  < 2e-16 ***
#   season5     -9.871e+04  2.326e+03 -42.443  < 2e-16 ***
#   season6     -1.083e+05  2.326e+03 -46.573  < 2e-16 ***
#   season7     -9.447e+04  2.326e+03 -40.612  < 2e-16 ***
#   season8     -9.791e+04  2.327e+03 -42.085  < 2e-16 ***
#   season9     -1.008e+05  2.327e+03 -43.334  < 2e-16 ***
#   season10    -9.195e+04  2.327e+03 -39.507  < 2e-16 ***
#   season11    -6.923e+04  2.375e+03 -29.146  < 2e-16 ***
#   season12    -1.795e+04  2.375e+03  -7.558 4.86e-12 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5928 on 140 degrees of freedom
# Multiple R-squared:  0.9796,	Adjusted R-squared:  0.9777 
# F-statistic: 516.7 on 13 and 140 DF,  p-value: < 2.2e-16

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
trend.season.pred <- forecast(trend.season, h = 14, level = c(0,95))
trend.season.pred
round(accuracy(trend.season.pred$fitted, vacation.ts),3)
# ME     RMSE      MAE    MPE MAPE  ACF1 Theil's U
# Test set  0 5652.507 4412.275 -0.045 4.52 0.603     0.257

##---ACF on residuals for full data-----------------
Acf(trend.season$residuals, lag.max = 12, 
    main = "Autocorrelation for Vacation Arrivals Full Data Residuals")

res.ar1 <- Arima(trend.season$residuals, order = c(1,0,0))
summary(res.ar1)
# Coefficients:
#   ar1       mean
# 0.6614  -299.6982
# s.e.  0.0648  1030.5226

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = 14, level = c(0,95))
res.ar1.pred

vacation.two.level.pred <- trend.season.pred$mean + res.ar1.pred$mean
vacation.two.level.pred

round(accuracy(trend.season.pred$fitted + res.ar1.pred$fitted, vacation.ts), 3)
#               ME     RMSE      MAE   MPE  MAPE  ACF1 Theil's U
# Test set 122.142 4370.613 3091.629 0.086 2.977 0.075     0.187

plot(trend.season.pred$fitted + res.ar1.pred$fitted, 
     xlab = "Time", ylab = "Vacation Arrivals", ylim = c(30000, 230000), bty = "l",
     xaxt = "n", xlim = c(2000, 2014), col = "blue",
     main = "Combined two-level Model for Full Data Vacation Arrivals") 
axis(1, at = seq(2000, 2014, 1), labels = format(seq(2000, 2014, 1)))
lines(vacation.two.level.pred, col = "blue", lwd = 2, lty =4)
lines(vacation.ts, col = "black", lwd = 1, lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(30000, 230000))
lines(c(2012.83, 2012.83), c(30000, 230000))
text(2005.5, 230000, "Historical data")
text(2013.5, 230000, "Forecast")

arrows(2012.75, 200000, 2000.25, 200000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2012.9, 200000, 2013.9, 200000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2005,70000, legend = c("Vacation Arrivals", "Vacation Arrivals Two-level model",
                              "Two-level forecast"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

point_forecast.df <- data.frame(vacation.two.level.pred)
names(point_forecast.df) <- c("Combined.Forecast")
point_forecast.df

#-----------------------------Auto-ARIMA model------------------------------------
# Use auto.arima() function to fit ARIMA model for entire set
# use summary() to show auto ARIMA model and its parameters for entire dataset.
vacation.auto.arima <- auto.arima(vacation.ts)
summary(vacation.auto.arima)
# ARIMA(2,1,2)(0,1,1)[12] 
# 
# Coefficients:
#   ar1     ar2     ma1      ma2     sma1
# -0.2785  0.4534  0.0667  -0.8873  -0.4198
# s.e.   0.0985  0.0981  0.0532   0.0531   0.0830

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
vacation.auto.arima.pred <- forecast(vacation.auto.arima, h = 14, level = 0)
vacation.auto.arima.pred

round(accuracy(vacation.auto.arima.pred$fitted,vacation.ts),3)
#                ME     RMSE      MAE    MPE  MAPE   ACF1 Theil's U
# Test set -697.598 4082.183 2881.249 -0.996 2.835 -0.017     0.181

plot(vacation.auto.arima.pred, 
     xlab = "Time", ylab = "Vacation Arrivals", ylim = c(30000, 230000), bty = "l",
     xaxt = "n", xlim = c(2000, 2014), 
     main = "Auto ARIMA Model for Vacation for Full data", flty = 2) 
axis(1, at = seq(2000, 2014, 1), labels = format(seq(2000, 2014, 1)))
lines(vacation.auto.arima.pred$fitted, col = "blue", lwd = 2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(30000, 230000))
lines(c(2012.83, 2012.83), c(30000, 230000))
text(2005.5, 230000, "Historical data")
text(2013.5, 230000, "Forecast")

arrows(2012.75, 200000, 2000.25, 200000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2012.9, 200000, 2013.9, 200000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2005,70000, legend = c("Vacation Arrival", "Auto ARIMA(2,1,2)(0,1,1)[12] model",
                             "Auto ARIMA forecast"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")




#HW model on full vacation data set with ZZZ
vacation_HW.ZZZ <- ets(vacation.ts, model = "ZZZ") 
summary(vacation_HW.ZZZ)


#ETS(M,A,M) 
#Call:
#  ets(y = arrivals.ts, model = "ZZZ") 
#Smoothing parameters:
#  alpha = 0.3138 
#beta  = 0.0049 
#gamma = 1e-04 
#Initial states:
#  l = 252795.1463 
#b = 1849.1426 
#s = 1.1846 1.0226 1.0783 0.9496 0.9291 1.0526
#0.8059 0.776 0.9365 0.971 1.0649 1.2289
#sigma:  0.041
#AIC     AICc      BIC 
#3732.303 3736.803 3783.931 
#Training set error measures:
#  ME    RMSE      MAE        MPE     MAPE      MASE       ACF1
#Training set -1412.396 12620.6 9542.076 -0.5109809 2.886038 0.5083675 0.05459384
#forecast for Nov 2012 to Dec 2014 using HW model
vacation_HW.pred <- forecast(vacation_HW.ZZZ, h = 14, level = c(0, 95))
vacation_HW.pred

round(accuracy(vacation_HW.pred$fitted, vacation.ts), 3)

plot(vacation_HW.pred, 
     xlab = "Time", ylab = "Vacation Arrivals", ylim = c(30000, 230000), bty = "l",
     xaxt = "n", xlim = c(2000, 2014), 
     main = "Auto ARIMA Model for Vacation for Full data", flty = 2) 
axis(1, at = seq(2000, 2014, 1), labels = format(seq(2000, 2014, 1)))
lines(vacation_HW.pred$fitted, col = "blue", lwd = 2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(30000, 230000))
lines(c(2012.83, 2012.83), c(30000, 230000))
text(2005.5, 230000, "Historical data")
text(2013.5, 230000, "Forecast")

arrows(2012.75, 200000, 2000.25, 200000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2012.9, 200000, 2013.9, 200000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2005,70000, legend = c("Vacation Arrival", "HW M Ad M model",
                              "HW forecast"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")


#snaive model
round(accuracy((snaive(vacation.ts))$fitted, vacation.ts), 3)
#Quadratic trend and seasonality model
round(accuracy(trend.season.pred$fitted + res.ar1.pred$fitted, vacation.ts),3)
#Auto-ARIMA model
round(accuracy(vacation.auto.arima.pred$fitted,vacation.ts),3)
#HW model
round(accuracy(vacation_HW.pred$fitted,vacation.ts),3)

#Combining all timeseries forecasts into same Dataframe
df.fitted <- data.frame(vacation_HW.pred$fitted,corporate_HW.ZZZ.pred$fitted,education.auto.arima.pred$fitted,vacation.auto.arima.pred$fitted)
names(df.fitted) <- c("arrivals","corporate","education","vacation")
df.fitted

df.forecasts <- data.frame(vacation_HW.pred$mean,corporate_HW.ZZZ.pred$mean,education.auto.arima.pred$mean,vacation.auto.arima.pred$mean)
names(df.forecasts) <- c("arr_hw","corp_hw","edu_arima","vac_arima")
df.forecasts

months <- c("Nov-2012","Dec-2012","Jan-2013","Feb-2013","Mar-2013","Apr-2013","May-2013","Jun-2013","Jul-2013","Aug-2013","Sep-2013","Oct-2013","Nov-2013","Dec-2013")
df.forecasts.months <- data.frame(months,df.forecasts)
names(df.forecasts.months) <- c("time","arr_hw","corp_hw","edu_arima","vac_arima")
df.forecasts.months

#install.packages("xlsx")
#install.packages("openxlsx")
library(xlsx)
library(openxlsx)
write.xlsx(df.fitted, "fitted_results.xlsx", sheetName="Fitted")
write.xlsx(df.forecasts, "forecast_results.xlsx", sheetName="Forecasts")

summary(df.fitted)
summary(df.forecasts)

#plotting all timeseries together
plot(df.fitted$arrivals, 
     xlab = "Time", ylab = "Arrivals", ylim = c(0, 550000), bty = "l",
     xaxt = "n", xlim = c(2000, 2014), 
     main = "Best Forecasts for Full data", lwd= 2, lty = 1, col = "black") 
axis(1, at = seq(2000, 2014, 1), labels = format(seq(2000, 2014, 1)))
lines(df.fitted$corporate, col = "red", lwd = 2,lty = 1)
lines(df.fitted$education, col = "green", lwd = 2,lty = 1)
lines(df.fitted$vacation, col = "blue", lwd = 2,lty = 1)
lines(df.forecasts$arr_hw, col = "black", lwd = 2,lty = 4)
lines(df.forecasts$corp_hw, col = "red", lwd = 2,lty = 4)
lines(df.forecasts$edu_arima, col = "green", lwd = 2,lty = 4)
lines(df.forecasts$vac_arima, col = "blue", lwd = 2,lty = 4)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(0, 550000))
lines(c(2012.83, 2012.83), c(0, 550000))
text(2006.5, 550000, "Historical data")
text(2013.5, 550000, "Forecast")

arrows(2012.75, 525000, 2000.25, 525000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2012.9, 525000, 2013.9, 525000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2001,500000, legend = c("Arrivals", "Corporate arrivals",
                              "Education arrivals","Vacation arrivals"), 
       col = c("black", "red" ,"green","blue"), 
       lty = c(1, 1, 1,1), lwd =c(2, 2, 2), bty = "o")

#plotting ONLY Education and Corporate arrivals together
plot(df.fitted$corporate, 
     xlab = "Time", ylab = "Arrivals", ylim = c(0, 18000), bty = "l",
     xaxt = "n", xlim = c(2000, 2014), 
     main = "Best Forecasts for Full data", lwd= 2, lty = 1, col = "red") 
axis(1, at = seq(2000, 2014, 1), labels = format(seq(2000, 2014, 1)))
lines(df.fitted$education, col = "green", lwd = 2,lty = 1)
lines(df.forecasts$corp_hw, col = "red", lwd = 2,lty = 4)
lines(df.forecasts$edu_arima, col = "green", lwd = 2,lty = 4)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(0, 18000))
lines(c(2012.83, 2012.83), c(0, 18000))
text(2006.5, 18000, "Historical data")
text(2013.5, 18000, "Forecast")

arrows(2012.75, 17500, 2000.25, 17500, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2012.9, 17500, 2013.9, 17500, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2001,500000, legend = c("Corporate arrivals",
                               "Education arrivals"), 
       col = c("red" ,"green"), 
       lty = c(1, 1), lwd =c(2, 2), bty = "o")
