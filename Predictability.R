library(forecast)

# set working directory for locating files.
setwd("C:/Users/saurabhn/Desktop/Snehaa/Time_Series_Analytics/Project/Project_Files_Collab/reban673timeseriesanalyticsgroupproject")

#setwd("E:/MS/Time Series Analytics/project")
#setwd("G:/CSUEB/MSBA/Spring 19/BAN_673_Forecasting/Project/")
# Create data frame.
nz.data <- read.csv("Processed_Raw_Dataset_NZ.csv")

# ----------<<<<ARRIVAL TIMESERIES>>>>-------------
arrivals.ts <- ts(nz.data$Arrivals, start = c(2000, 1), end = c(2012, 10), freq = 12)
arrivals.stl <- stl(arrivals.ts, s.window = "periodic")
autoplot(arrivals.stl, main = "NZ Arrivals Time Series Component")
plot(arrivals.ts,
     xlab="Time",ylab="Passengers Arrivals",
     ylim = c(200000, 500000),
     main = "NZ Arrivals",col='blue')

autocor <- Acf(arrivals.ts, lag.max = 12, main = "Autocorrelation for NZ Arrivals")
Lag_arrivals <- round(autocor$lag, 0)
ACF_arrivals <- round(autocor$acf, 3)
data.frame(Lag_arrivals, ACF_arrivals)
#  Lag_arrivals ACF_arrivals
#1              0         1.000
#2              1         0.747
#3              2         0.567
#4              3         0.502
#5              4         0.256
#6              5         0.210
#7              6         0.286
#8              7         0.200
#9              8         0.223
#10             9         0.425
#11            10         0.469
#12            11         0.613
#13            12         0.804

diff.arrivals <- diff(arrivals.ts, lag = 1)
Acf(diff.arrivals, lag.max = 12, 
    main = "Autocorrelation for Arrivals timeseries")

arrivals.ar1<- Arima(arrivals.ts, order = c(1,0,0))
summary(arrivals.ar1)
# Coefficients:
#   ar1       mean
# 0.7548  345966.68
# s.e.  0.0527   13912.23

# ----------<<<<CORPORATE TIMESERIES>>>>-------------
corporate.ts <- ts(nz.data$Corporate, start = c(2000, 1), end = c(2012, 10), freq = 12)
corporate.stl <- stl(corporate.ts, s.window = "periodic")
autoplot(corporate.stl, main = "NZ Arrivals For Corporate Reason Time Series Component")

# Use acf() function to identify autocorrealtion and plot autocorrrelation
# for different lags (up to maximum of 12).
autocor <- Acf(corporate.ts, lag.max = 12, main = "Autocorrelation for Corporate Arrivals")

# Display autocorrelatiion coefficients for various lags
Lag_corporate <- round(autocor$lag, 0)
ACF_corporate <- round(autocor$acf, 3)
data.frame(Lag_corporate, ACF_corporate)

#Lag_corporate ACF_corporate
#1              0         1.000
#2              1         0.625
#3              2         0.311
#4              3         0.327
#5              4         0.271
#6              5         0.207
#7              6         0.196
#8              7         0.155
#9              8         0.182
#10             9         0.174
#11            10         0.124
#12            11         0.393
#13            12         0.672

diff.corporate <- diff(corporate.ts, lag = 1)
Acf(diff.corporate, lag.max = 12, 
    main = "Autocorrelation for Corporate timeseries")

corporate.ar1<- Arima(corporate.ts, order = c(1,0,0))
summary(corporate.ar1)
# Coefficients:
#   ar1       mean
# 0.6327  8815.2976
# s.e.  0.0626   202.6649

# ----------<<<<EDUCATION TIMESERIES>>>>-------------
education.ts <- ts(nz.data$Education, 
                   start = c(2000, 1), end = c(2012, 10), freq = 12)
education.stl <- stl(education.ts, s.window = "periodic")
autoplot(education.stl, main = "NZ Arrivals For Education Reason Time Series Component")

# Use acf() function to identify autocorrealtion and plot autocorrrelation
# for different lags (up to maximum of 12).
autocor <- Acf(education.ts, lag.max = 12, main = "Autocorrelation for Student Arrivals")

# Display autocorrelatiion coefficients for various lags
Lag_education <- round(autocor$lag, 0)
ACF_education <- round(autocor$acf, 3)
data.frame(Lag_education, ACF_education)

#Lag_education ACF_education
#1              0         1.000
#2              1         0.819
#3              2         0.619
#4              3         0.571
#5              4         0.551
#6              5         0.479
#7              6         0.400
#8              7         0.391
#9              8         0.381
#10             9         0.323
#11            10         0.296
#12            11         0.405
#13            12         0.469

diff.education <- diff(education.ts, lag = 1)
Acf(diff.education, lag.max = 12, 
    main = "Autocorrelation for Education timeseries")

education.ar1<- Arima(education.ts, order = c(1,0,0))
summary(education.ar1)
# Coefficients:
#   ar1        mean
# 0.8822  12052.1168
# s.e.  0.0422    892.5254

# ----------<<<<VACATION TIMESERIES>>>>-------------
vacation.ts <- ts(nz.data$Vacation, 
                  start = c(2000, 1), end = c(2012, 10), freq = 12)
vacation.stl <- stl(vacation.ts, s.window = "periodic")
autoplot(vacation.stl, main = "NZ Arrivals For Vacation Reason Time Series Component")

# Use acf() function to identify autocorrealtion and plot autocorrrelation
# for different lags (up to maximum of 12).
autocor <- Acf(vacation.ts, lag.max = 12, main = "Autocorrelation for Tourist Arrivals")

# Display autocorrelatiion coefficients for various lags
Lag_vacation <- round(autocor$lag, 0)
ACF_vacation <- round(autocor$acf, 3)
data.frame(Lag_vacation, ACF_vacation)
#Lag_vacation ACF_vacation
#1             0        1.000
#2             1        0.818
#3             2        0.412
#4             3       -0.028
#5             4       -0.383
#6             5       -0.588
#7             6       -0.655
#8             7       -0.600
#9             8       -0.408
#10            9       -0.067
#11           10        0.359
#12           11        0.741
#13           12        0.904

diff.vacation <- diff(vacation.ts, lag = 1)
Acf(diff.vacation, lag.max = 12, 
    main = "Autocorrelation for Vacation timeseries")

vacation.ar1<- Arima(vacation.ts, order = c(1,0,0))
summary(vacation.ar1)
# Coefficients:
#   ar1        mean
# 0.8159  106989.328
# s.e.  0.0455    9651.715