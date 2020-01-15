library(forecast)

# set working directory for locating files.
setwd("C:/Users/saurabhn/Desktop/Snehaa/Time_Series_Analytics/Project/Project_Files_Collab/reban673timeseriesanalyticsgroupproject")
#setwd("E:/MS/Time Series Analytics/project")
#setwd("G:/CSUEB/MSBA/Spring 19/BAN_673_Forecasting/Project/")
# Create data frame.
nz.data <- read.csv("Processed_Raw_Dataset_NZ.csv")

corporate.ts <- ts(nz.data$Corporate, start = c(2000, 1), end = c(2012, 10), freq = 12)
# ----------<<<<CORPORATE DATASET>>>>-------------

## Use plot() to plot time series data  
plot(corporate.ts, 
     xlab = "Time", ylab = "Arrivals", 
     ylim = c(6000, 12000), main = "Corporate Travelers By Time Period", col = "blue")

# Create fixed data partitioining for data.
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 22
nTrain <- length(corporate.ts) - nValid
train_corporate.ts <- window(corporate.ts, start = c(2000, 1), end = c(2000, nTrain))
valid_corporate.ts <- window(corporate.ts, start = c(2000, nTrain + 1), 
                             end = c(2000, nTrain + nValid))

# Create Holt-Winter's exponenthial smoothing (HW) for Amtrak data.
# Use ets() function with model = "ZZZ", i.e., automated selection 
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw_corporate.ZZZ <- ets(train_corporate.ts, model = "ZZZ") 
hw_corporate.ZZZ # Model appears to be (A, N, A), with alpha = 0.6619 and gamma=1e-4

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw_corporate.ZZZ.pred <- forecast(hw_corporate.ZZZ, h = nValid, level = 0)
hw_corporate.ZZZ.pred

round(accuracy(hw_corporate.ZZZ.pred, valid_corporate.ts), 3)
#                    ME    RMSE     MAE    MPE  MAPE  MASE  ACF1 Theil's U
# Training set    7.792 418.775 322.159 -0.013 3.745 0.465 0.007        NA
# Test set     -203.068 414.406 284.240 -2.572 3.489 0.410 0.370     0.355


plot(hw_corporate.ZZZ.pred, 
     xlab = "Time", ylab = "Corporate Arrivals", ylim = c(5000, 13500), bty = "l",
     xaxt = "n", xlim = c(2000, 2013), 
     main = "Holt-Winter's Model for Corporate Travelers with Automated Selection\n of Model Options", flty = 2) 
axis(1, at = seq(2000, 2013, 1), labels = format(seq(2000, 2013, 1)))
lines(hw_corporate.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(valid_corporate.ts)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(5000, 13500))
lines(c(2011, 2011), c(5000, 13500))
text(2005.5, 13000, "Training")
text(2012, 13000, "Validation")

arrows(2010.75, 12500, 2000.25, 12500, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2011.25, 12500, 2012.75, 12500, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2003,7000, legend = c("Corporate Arrival", "Corporate Arrival HW's (A N A) model",
                             "HW's forecast for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#------------------Regression Model with Quadratic Trend & Seasonality-------------------
# Use tslm() function to create quadratic trend and seasonal model.
train.trend.season <- tslm(train_corporate.ts ~ trend + I(trend^2) + season)
summary(train.trend.season)#seasons 5,8,9,10,12 insignificant p-values
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 6353.46421  202.02306  31.449  < 2e-16 ***
#   trend         63.51304    4.80755  13.211  < 2e-16 ***
#   I(trend^2)    -0.35498    0.03501 -10.140  < 2e-16 ***
#   season2     2084.05813  222.69080   9.359 6.61e-16 ***
#   season3     1791.46257  222.70108   8.044 7.61e-13 ***
#   season4      451.57697  222.71788   2.028  0.04486 *  
#   season5      -13.78050  222.74100  -0.062  0.95077    
# season6     -684.88256  222.77031  -3.074  0.00262 ** 
#   season7     -459.36558  222.80573  -2.062  0.04143 *  
#   season8     -151.59319  222.84727  -0.680  0.49767    
# season9     -107.38357  222.89498  -0.482  0.63086    
# season10     426.44509  222.94900   1.913  0.05820 .  
# season11    1297.62006  223.00952   5.819 5.19e-08 ***
#   season12    -131.13138  223.07679  -0.588  0.55777    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 522.2 on 118 degrees of freedom
# Multiple R-squared:  0.841,	Adjusted R-squared:  0.8235 
# F-statistic:    48 on 13 and 118 DF,  p-value: < 2.2e-16

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)
train.trend.season.pred
round(accuracy(train.trend.season.pred, valid_corporate.ts),3)
#                   ME    RMSE     MAE    MPE  MAPE  MASE  ACF1 Theil's U
# Training set   0.000 493.777 402.562 -0.292 4.538 0.581 0.615        NA
# Test set     169.527 534.827 476.851  1.767 5.704 0.688 0.591     0.454

#-------------------ACF on residuals data-----------------
Acf(train.trend.season$residuals, lag.max = 12, 
    main = "Autocorrelation for Corporate Arrivals Training Residuals")
# Acf(train.trend.season.pred$residuals, lag.max = 12, 
#     main = "Autocorrelation for Arrivals pred Residuals")
Acf(valid_corporate.ts - train.trend.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Corporate Validation Residuals")

res.ar1 <- Arima(train.trend.season$residuals, order = c(1,0,0))
summary(res.ar1)
# ARIMA(1,0,0) with non-zero mean 
# 
# Coefficients:
#   ar1     mean
# 0.6146   1.9629
# s.e.  0.0681  86.6971
# 
# sigma^2 estimated as 153277:  log likelihood=-974.57
# AIC=1955.14   AICc=1955.33   BIC=1963.79
# 
# Training set error measures:
#                     ME     RMSE      MAE       MPE     MAPE      MASE        ACF1
# Training set -2.485922 388.5288 305.1337 -27.24521 216.8904 0.4978519 -0.06198929

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

valid.two.level.pred <- train.trend.season.pred$mean + res.ar1.pred$mean

round(accuracy(valid.two.level.pred, valid_corporate.ts), 3)
#               ME    RMSE     MAE   MPE  MAPE  ACF1 Theil's U
# Test set 184.589 525.424 473.729 1.939 5.644 0.589     0.452

plot(train.trend.season.pred$fitted + res.ar1.pred$fitted, 
     xlab = "Time", ylab = "Corporate Arrivals", ylim = c(5000, 13500), bty = "l",
     xaxt = "n", xlim = c(2000, 2013), col = "blue",
     main = "Combined two-level Model for Corporate Arrivals") 
axis(1, at = seq(2000, 2013, 1), labels = format(seq(2000, 2013, 1)))
lines(valid.two.level.pred, col = "blue", lwd = 2, lty =4)
lines(train_corporate.ts, col = "black", lwd = 1, lty = 1)
lines(valid_corporate.ts, col = "black", lwd = 1, lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(5000, 13500))
lines(c(2011, 2011), c(5000, 13500))
text(2005.5, 13000, "Training")
text(2012, 13000, "Validation")

arrows(2010.75, 12500, 2000.25, 12500, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2011.25, 12500, 2012.75, 12500, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2003,7000, legend = c("Corporate Arrival", "Corporate Arrival Two-level model",
                             "Two-level forecast for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#-------------------AUTO ARIMA-------------------
# Use auto.arima() function to fit ARIMA model.
# use summary() to show auto ARIMA model and its parameters for entire dataset.
train_corporate.auto.arima <- auto.arima(train_corporate.ts)
summary(train_corporate.auto.arima)
# ARIMA(0,1,1)(0,1,1)[12] 
# 
# Coefficients:
#   ma1     sma1
# -0.3954  -0.7949
# s.e.   0.0957   0.1074

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train_corporate.auto.arima.pred <- forecast(train_corporate.auto.arima, h = nValid, level = 0)
summary(train_corporate.auto.arima.pred)

round(accuracy(train_corporate.auto.arima.pred, valid_corporate.ts), 3)
#                    ME    RMSE     MAE    MPE  MAPE  MASE  ACF1 Theil's U
# Training set  -13.638 435.384 326.603 -0.312 3.654 0.471 0.030        NA
# Test set     -278.067 479.975 328.439 -3.452 4.015 0.474 0.354     0.402

plot(train_corporate.auto.arima.pred, 
     xlab = "Time", ylab = "Corporate Arrivals", ylim = c(5000, 13500), bty = "l",
     xaxt = "n", xlim = c(2000, 2013), 
     main = "Auto ARIMA Model for Corporate Arrivals", flty = 2) 
axis(1, at = seq(2000, 2013, 1), labels = format(seq(2000, 2013, 1)))
lines(train_corporate.auto.arima$fitted, col = "blue", lwd = 2)
lines(valid_corporate.ts)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(5000, 13500))
lines(c(2011, 2011), c(5000, 13500))
text(2005.5, 13000, "Training")
text(2012, 13000, "Validation")

arrows(2010.75, 12500, 2000.25, 12500, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2011.25, 12500, 2012.75, 12500, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2001,7000, legend = c("Corporate Arrival", "Corporate Arrival Auto-ARIMA (0,1,1)(0,1,1)[12] model",
                             "Auto-ARIMA forecast for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#------------------Comparing Validation performance
round(accuracy(snaive(train_corporate.ts), valid_corporate.ts), 3)
round(accuracy(hw_corporate.ZZZ.pred, valid_corporate.ts), 3)
round(accuracy(train.trend.season.pred, valid_corporate.ts),3)
round(accuracy(valid.two.level.pred, valid_corporate.ts), 3)
round(accuracy(train_corporate.auto.arima.pred, valid_corporate.ts), 3)


#####################################################################
##################---------ENTIRE DATA SET--------###################
#####################################################################
#-----------------------------HW model-----------------------------------------
#HW model on full data set with ZZZ
corporate_HW.ZZZ <- ets(corporate.ts, model = "ZZZ") 
summary(corporate_HW.ZZZ)
# ETS(A,N,A) 
# 
# Call:
#   ets(y = corporate.ts, model = "ZZZ") 
# 
# Smoothing parameters:
#   alpha = 0.6571 
# gamma = 1e-04 
# 
# Initial states:
#   l = 8229.2793 
# s = -465.2379 960.7607 78.0149 -433.3355 -502.646 -803.8072
# -1100.281 -419.7872 92.5321 1469.799 1636.277 -512.2895
# 
# sigma:  434.8717
# 
# AIC     AICc      BIC 
# 2662.129 2665.607 2707.683 
# 
# Training set error measures:
#   ME     RMSE      MAE         MPE     MAPE      MASE      ACF1
# Training set 5.027211 414.6339 316.9557 -0.06256904 3.690328 0.4627471 0.0300756

#forecast for Nov 2012 to Dec 2014 using HW model
corporate_HW.ZZZ.pred <- forecast(corporate_HW.ZZZ, h = 14, level = c(0, 95))
corporate_HW.ZZZ.pred

round(accuracy(corporate_HW.ZZZ.pred$fitted, corporate.ts), 3)
#             ME    RMSE     MAE    MPE MAPE ACF1 Theil's U
# Test set 5.027 414.634 316.956 -0.063 3.69 0.03     0.401

plot(corporate_HW.ZZZ.pred, 
     xlab = "Time", ylab = "Corporate Arrivals", ylim = c(5000, 13500), bty = "l",
     xaxt = "n", xlim = c(2000, 2014), 
     main = "Holt-Winter's Model Forecast for Corporate with Automated Selection", flty = 2) 
axis(1, at = seq(2000, 2014, 1), labels = format(seq(2000, 2014, 1)))
lines(corporate_HW.ZZZ.pred$fitted, col = "blue", lwd = 2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(5000, 13500))
lines(c(2012.83, 2012.83), c(5000, 13500))
text(2005.5, 13000, "Historical data")
text(2013.5, 13000, "Forecast")

arrows(2012.75, 12500, 2000.25, 12500, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2012.9, 12500, 2013.9, 12500, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2003,7000, legend = c("Corporate Arrival", "Corporate Arrival HW's (A,N,A) model",
                             "HW's forecast"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#-----------------------------Auto-ARIMA model------------------------------------
# Use auto.arima() function to fit ARIMA model for entire set
# use summary() to show auto ARIMA model and its parameters for entire dataset.
corporate.auto.arima <- auto.arima(corporate.ts)
summary(corporate.auto.arima)
# ARIMA(0,1,1)(1,1,0)[12] 
# 
# Coefficients:
#   ma1     sar1
# -0.3959  -0.5033
# s.e.   0.0815   0.0744
# 
# sigma^2 estimated as 261447:  log likelihood=-1080.32
# AIC=2166.63   AICc=2166.81   BIC=2175.48

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
corporate.auto.arima.pred <- forecast(corporate.auto.arima, h = 14, level = c(0,95))
corporate.auto.arima.pred

round(accuracy(corporate.auto.arima.pred$fitted,corporate.ts),3)
#              ME    RMSE     MAE    MPE  MAPE  ACF1 Theil's U
# Test set -2.735 485.779 366.706 -0.155 4.132 0.014     0.461

plot(corporate.auto.arima.pred, 
     xlab = "Time", ylab = "Corporate Arrivals", ylim = c(5000, 13500), bty = "l",
     xaxt = "n", xlim = c(2000, 2014), 
     main = "Auto ARIMA Model for Corporate Arrivals", flty = 2) 
axis(1, at = seq(2000, 2014, 1), labels = format(seq(2000, 2014, 1)))
lines(corporate.auto.arima.pred$fitted, col = "blue", lwd = 2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(5000, 13500))
lines(c(2012.83, 2012.83), c(5000, 13500))
text(2005.5, 13000, "Historical data")
text(2013.5, 13000, "Forecast")

arrows(2012.75, 12500, 2000.25, 12500, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2012.9, 12500, 2013.9, 12500, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2003,7000, legend = c("Corporate Arrival", "Auto ARIMA(0,1,1)(1,1,0)[12] model",
                             "Auto ARIMA forecast"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#snaive model
round(accuracy((snaive(corporate.ts))$fitted, corporate.ts), 3)
#HW's auto-select model
round(accuracy(corporate_HW.ZZZ.pred$fitted, corporate.ts),3)
#Aut-ARIMA model
round(accuracy(corporate.auto.arima.pred$fitted,corporate.ts),3)
