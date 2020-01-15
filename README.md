# BAN673-Time-Series-Analytics-Project
Utilized multiple time series forecasting methods (auto-regression/Holt-Winter’s/ARIMA Models) to forecast New Zealand tourist numbers and their broad purpose of visit 14 months into the future

# Project Summary:
New Zealand government’s problem statement to analytically budget their 2013 marketing spend of 100 million $ required their understanding of the trends and patterns both in the visitor numbers as well as their purpose of visit. This will help them obtain the forecasts and
plan the monthly advertising budget and advertising content focus (Business, Education, Vacation etc.). There is data available monthly from Jan 2000 to Oct 2012.  

They contracted ELSSA analytics to analyse the data and predict up to 14 months in future. Our objective was to analyse various time series methods and select the best method to forecast visitor arrivals, the number of visitors and their broad purpose of visit with the given
data. From this we could suggest which category of NZ Tourism advertisement could be allocated how much budget to run for each month of the year 2013 with what percentage of their marketing budget.  

The broad approach here was to employ several time series forecasting methods on training and validation partitions and compare the accuracy error measures for their models. The best models were then extended to full historical data and checked for compared for accuracy with respect to full historical data. The best performing method was used to forecast the future data and allocate marketing budgets.
The exercise revealed that various time series, though related to visitor inflow exhibited varied trends and seasonal patterns when visualized. They also required different forecasting methods to efficiently make future predictions. The Arrival and Corporate time series
demanded the use of a different forecasting approach than those of Education and Vacation time series and concluded with prediction accuracies of varied metrics. Upon successful forecasting, interesting trends and contrasting seasonality were revealed for different visitor categories which helped optimize marketing spend and advertising budget allocation.  

