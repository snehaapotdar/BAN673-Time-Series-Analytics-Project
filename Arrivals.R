library(forecast)

# set working directory for locating files.
setwd("C:/Users/saurabhn/Desktop/Snehaa/Time_Series_Analytics/Project/Project_Files_Collab/reban673timeseriesanalyticsgroupproject")
#setwd("E:/MS/Time Series Analytics/project")
#setwd("G:/CSUEB/MSBA/Spring 19/BAN_673_Forecasting/Project/")
# Create data frame.
nz.data <- read.csv("Processed_Raw_Dataset_NZ.csv")

# ----------<<<<ARRIVAL DATASET>>>>-------------
arrivals.ts <- ts(nz.data$Arrivals, start = c(2000, 1), end = c(2012, 10), freq = 12)

## Use plot() to plot time series data  
plot(arrivals.ts, 
     xlab = "Time", ylab = "Arrivals", 
     ylim = c(200000, 500000), main = "NZ Arrivals", col = "blue")

# Create fixed data partitioining for data.
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 22
nTrain <- length(arrivals.ts) - nValid
train_arrivals.ts <- window(arrivals.ts, start = c(2000, 1), end = c(2000, nTrain))
valid_arrivals.ts <- window(arrivals.ts, start = c(2000, nTrain + 1), 
                            end = c(2000, nTrain + nValid))

# Create Holt-Winter's exponenthial smoothing (HW).
# Use ets() function with model = "ZZZ", i.e., automated selection 
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw_arrivals.ZZZ <- ets(train_arrivals.ts, model = "ZZZ") 
hw_arrivals.ZZZ # Model appears to be (M, Ad, M), with alpha = 0.3808,beta=0.0023 gamma = 1e-4 and gamma 0.98.

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.

hw_arrivals.ZZZ.pred <- forecast(hw_arrivals.ZZZ, h = nValid, level =0 )
hw_arrivals.ZZZ.pred
length(valid_arrivals.ts)
length(hw_arrivals.ZZZ.pred)
#Testing validation data
round(accuracy(hw_arrivals.ZZZ.pred, valid_arrivals.ts), 3)
#                    ME     RMSE       MAE   MPE  MAPE  MASE   ACF1 Theil's U
# Training set  640.896 11919.90  9002.753 0.049 2.808 0.475 -0.037        NA
# Test set     4970.039 16041.38 12050.561 1.113 3.088 0.636  0.238     0.286
#Testing Full data
round(accuracy(hw_arrivals.ZZZ.pred$fitted, arrivals.ts), 3)
#               ME    RMSE      MAE   MPE  MAPE   ACF1 Theil's U
# Test set 640.896 11919.9 9002.753 0.049 2.808 -0.037      0.26

plot(hw_arrivals.ZZZ.pred, 
     xlab = "Time", ylab = "Arrivals", ylim = c(200000, 550000), bty = "l",
     xaxt = "n", xlim = c(2000, 2013), 
     main = "Holt-Winter's Model for Arrivals with Automated Selection of Model Options", flty = 2) 
axis(1, at = seq(2000, 2013, 1), labels = format(seq(2000, 2013, 1)))
lines(hw_arrivals.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(valid_arrivals.ts)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(200000, 550000))
lines(c(2011, 2011), c(200000, 550000))
text(2005.5, 550000, "Training")
text(2012, 550000, "Validation")

arrows(2010.75, 525000, 2000.25, 525000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2011.25, 525000, 2012.75, 525000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2005,270000, legend = c("Arrival", "Arrival HW's (M Ad M) model",
                               "HW's forecast for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")
#------------------Regression Model with Quadratic Trend & Seasonality-------------------
# Use tslm() function to create quadratic trend and seasonal model.
train.trend.season <- tslm(train_arrivals.ts ~ trend + I(trend^2) + season)
summary(train.trend.season)
# Call:
#   tslm(formula = train_arrivals.ts ~ trend + I(trend^2) + season)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -39621  -9347    633   9524  41865 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  3.101e+05  6.268e+03  49.475  < 2e-16 ***
#   trend        2.353e+03  1.492e+02  15.774  < 2e-16 ***
#   I(trend^2)  -9.304e+00  1.086e+00  -8.566 4.75e-14 ***
#   season2     -5.178e+04  6.909e+03  -7.494 1.34e-11 ***
#   season3     -8.108e+04  6.909e+03 -11.735  < 2e-16 ***
#   season4     -9.741e+04  6.910e+03 -14.098  < 2e-16 ***
#   season5     -1.492e+05  6.910e+03 -21.591  < 2e-16 ***
#   season6     -1.388e+05  6.911e+03 -20.089  < 2e-16 ***
#   season7     -5.599e+04  6.912e+03  -8.100 5.67e-13 ***
#   season8     -1.012e+05  6.914e+03 -14.637  < 2e-16 ***
#   season9     -9.391e+04  6.915e+03 -13.580  < 2e-16 ***
#   season10    -4.749e+04  6.917e+03  -6.865 3.30e-10 ***
#   season11    -6.776e+04  6.919e+03  -9.793  < 2e-16 ***
#   season12    -1.233e+04  6.921e+03  -1.781   0.0775 .
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 16200 on 118 degrees of freedom
# Multiple R-squared:  0.9423,	Adjusted R-squared:  0.9359
# F-statistic: 148.2 on 13 and 118 DF,  p-value: < 2.2e-16

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)
train.trend.season.pred
round(accuracy(train.trend.season.pred, valid_arrivals.ts),3)
#                    ME     RMSE      MAE    MPE  MAPE  MASE  ACF1 Theil's U
# Training set     0.00 15319.32 12075.21 -0.182 3.866 0.637 0.529        NA
# Test set     17842.75 25468.95 21211.08  4.172 5.180 1.119 0.385     0.443

#-------------------ACF on residuals data-----------------
Acf(train.trend.season$residuals, lag.max = 12, 
    main = "Autocorrelation for Arrivals Training Residuals")
# Acf(train.trend.season.pred$residuals, lag.max = 12, 
#     main = "Autocorrelation for Arrivals pred Residuals")
Acf(valid_arrivals.ts - train.trend.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Arrivals Validation Residuals")

res.ar1 <- Arima(train.trend.season$residuals, order = c(1,0,0))
summary(res.ar1)
?Arima
# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

valid.two.level.pred <- train.trend.season.pred$mean + res.ar1.pred$mean

round(accuracy(valid.two.level.pred, valid_arrivals.ts), 3)
#                ME     RMSE      MAE   MPE  MAPE ACF1 Theil's U
# Test set 16272.81 25241.94 21082.93 3.795 5.186 0.47     0.446

plot(train.trend.season.pred$fitted + res.ar1.pred$fitted, 
     xlab = "Time", ylab = "Arrivals", ylim = c(200000, 550000), bty = "l",
     xaxt = "n", xlim = c(2000, 2013), col = "blue",
     main = "Combined two-level Model for Arrivals") 
axis(1, at = seq(2000, 2013, 1), labels = format(seq(2000, 2013, 1)))
lines(valid.two.level.pred, col = "blue", lwd = 2, lty =4)
lines(train_arrivals.ts, col = "black", lwd = 1, lty = 1)
lines(valid_arrivals.ts, col = "black", lwd = 1, lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(200000, 550000))
lines(c(2011, 2011), c(200000, 550000))
text(2005.5, 550000, "Training")
text(2012, 550000, "Validation")

arrows(2010.75, 525000, 2000.25, 525000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2011.25, 525000, 2012.75, 525000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2000.1,510000, legend = c("Arrival", "Combined two-level model",
                               "Combined 2-level forecast for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")


#-------------------AUTO ARIMA-------------------
# Use auto.arima() function to fit ARIMA model.
# use summary() to show auto ARIMA model and its parameters for entire dataset.
train_arrivals.auto.arima <- auto.arima(train_arrivals.ts)
summary(train_arrivals.auto.arima)
nValid
# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train_arrivals.auto.arima.pred <- forecast(train_arrivals.auto.arima, h = nValid, level = 0)
summary(train_arrivals.auto.arima.pred)

round(accuracy(train_arrivals.auto.arima.pred, valid_arrivals.ts), 3)
#                     ME     RMSE       MAE    MPE  MAPE  MASE   ACF1 Theil's U
# Training set -1129.643 13335.14  9144.996 -0.531 2.806 0.483 -0.013        NA
# Test set     -6079.608 16849.09 14123.861 -1.878 3.725 0.745  0.204     0.305

plot(train_arrivals.auto.arima.pred, 
     xlab = "Time", ylab = "Arrivals", ylim = c(200000, 550000), bty = "l",
     xaxt = "n", xlim = c(2000, 2013), 
     main = "Auto ARIMA Model for Arrivals", flty = 2) 
axis(1, at = seq(2000, 2013, 1), labels = format(seq(2000, 2013, 1)))
lines(train_arrivals.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid_arrivals.ts)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(200000, 550000))
lines(c(2011, 2011), c(200000, 550000))
text(2005.5, 550000, "Training")
text(2012, 550000, "Validation")

arrows(2010.75, 525000, 2000.25, 525000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2011.25, 525000, 2012.75, 525000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2000.1,510000, legend = c("Arrival", "Auto ARIMA(0,1,1)(0,1,1)[12] model",
                               "Auto ARIMA forecast for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#------------------Comparing Validation performance
round(accuracy(snaive(train_arrivals.ts), valid_arrivals.ts), 3)
round(accuracy(hw_arrivals.ZZZ.pred, valid_arrivals.ts), 3)
round(accuracy(train.trend.season.pred, valid_arrivals.ts),3)
round(accuracy(valid.two.level.pred, valid_arrivals.ts), 3)
round(accuracy(train_arrivals.auto.arima.pred, valid_arrivals.ts), 3)

#####################################################################
##################---------ENTIRE DATA SET--------###################
#####################################################################
#-----------------------------HW model-----------------------------------------
#HW model on full data set with ZZZ
arrivals_HW.ZZZ <- ets(arrivals.ts, model = "ZZZ") 
summary(arrivals_HW.ZZZ)
# ETS(M,A,M) 
# 
# Call:
#   ets(y = arrivals.ts, model = "ZZZ") 
# 
# Smoothing parameters:
#   alpha = 0.3138 
# beta  = 0.0049 
# gamma = 1e-04 
# 
# Initial states:
#   l = 252795.1463 
# b = 1849.1426 
# s = 1.1846 1.0226 1.0783 0.9496 0.9291 1.0526
# 0.8059 0.776 0.9365 0.971 1.0649 1.2289
# 
# sigma:  0.041
# 
# AIC     AICc      BIC 
# 3732.303 3736.803 3783.931 
# 
# Training set error measures:
#   ME    RMSE      MAE        MPE     MAPE      MASE       ACF1
# Training set -1412.396 12620.6 9542.076 -0.5109809 2.886038 0.5083675 0.05459384


#forecast for Nov 2012 to Dec 2014 using HW model
arrivals_HW.pred <- forecast(arrivals_HW.ZZZ, h = 14, level = c(0, 95))
arrivals_HW.pred

round(accuracy(arrivals_HW.pred$fitted, arrivals.ts), 3)
#                 ME    RMSE      MAE    MPE  MAPE  ACF1 Theil's U
# Test set -1412.396 12620.6 9542.076 -0.511 2.886 0.055     0.263
plot(arrivals_HW.pred, 
     xlab = "Time", ylab = "Arrivals", ylim = c(200000, 550000), bty = "l",
     xaxt = "n", xlim = c(2000, 2014), 
     main = "Holt-Winter's Model Forecast for Arrivals with Automated Selection", flty = 2) 
axis(1, at = seq(2000, 2014, 1), labels = format(seq(2000, 2014, 1)))
lines(arrivals_HW.pred$fitted, col = "blue", lwd = 2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(200000, 550000))
lines(c(2012.83, 2012.83), c(200000, 550000))
text(2005.5, 550000, "Historical data")
text(2013.5, 550000, "Forecast")

arrows(2012.75, 525000, 2000.25, 525000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2012.9, 525000, 2013.9, 525000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2005,275000, legend = c("Arrival", "Arrival HW's (M A M) model",
                               "HW's forecast"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#-----------------------------Auto-ARIMA model------------------------------------
# Use auto.arima() function to fit ARIMA model for entire set
# use summary() to show auto ARIMA model and its parameters for entire dataset.
arrivals.auto.arima <- auto.arima(arrivals.ts)
summary(arrivals.auto.arima)
# Series: arrivals.ts 
# ARIMA(3,1,2)(2,1,1)[12] 
# Coefficients:
#   ar1      ar2     ar3      ma1    ma2     sar1    sar2    sma1
# -0.1735  -0.3589  0.0205  -0.3948  0.104  -0.6506  -0.389  0.0852
# s.e.      NaN   0.0083     NaN      NaN    NaN      NaN     NaN     NaN
# 
# sigma^2 estimated as 216357972:  log likelihood=-1552.14
# AIC=3122.28   AICc=3123.65   BIC=3148.82
# 
# Training set error measures:
#   ME     RMSE      MAE        MPE     MAPE      MASE       ACF1
# Training set -864.1625 13669.48 9661.433 -0.4132111 2.905561 0.5147264 0.00749216


# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 14 periods. 
arrivals.auto.arima.pred <- forecast(arrivals.auto.arima, h = 14, level = c(0, 95))
arrivals.auto.arima.pred

round(accuracy(arrivals.auto.arima.pred$fitted,arrivals.ts),3)

plot(arrivals.auto.arima.pred, 
     xlab = "Time", ylab = "Arrivals", ylim = c(200000, 550000), bty = "l",
     xaxt = "n", xlim = c(2000, 2014), 
     main = "Auto ARIMA Model for Arrivals", flty = 2) 
axis(1, at = seq(2000, 2014, 1), labels = format(seq(2000, 2014, 1)))
lines(arrivals.auto.arima.pred$fitted, col = "blue", lwd = 2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2000, 2000), c(200000, 550000))
lines(c(2012.83, 2012.83), c(200000, 550000))
text(2005.5, 550000, "Historical data")
text(2013.5, 550000, "Forecast")

arrows(2012.75, 525000, 2000.25, 525000, code = 3, length = 0.1,lwd = 1, angle = 30)
arrows(2012.9, 525000, 2013.9, 525000, code = 3, length = 0.1,lwd = 1, angle = 30)

legend(2005,275000, legend = c("Arrival", "Auto ARIMA(3,1,2)(2,1,1)[12] model",
                               "Auto ARIMA forecast"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "o")

#ARIMA model
round(accuracy(arrivals.auto.arima.pred$fitted,arrivals.ts),3)
#HW's auto selection model
round(accuracy(arrivals_HW.pred$fitted, arrivals.ts),3)
#snaive model
round(accuracy((snaive(arrivals.ts))$fitted, arrivals.ts), 3)

