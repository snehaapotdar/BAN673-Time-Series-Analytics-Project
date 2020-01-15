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

# Introduction:

New Zealand government recently released data for NZ Air passengers from incoming and outgoing passengers. It also released other monthly data for Visitor Purposes. The Tourism department has decided to understand the trends and patterns in the visitor travel information in relation to their purpose. ELSSA analytics has been contracted to analyse the data and bring out trends, seasonality patterns in these using historical data and forecast for the period upto Dec 2013. At ELSSA Analytics, they specialize in time series forecasting using various methods like Smoothing, Regression, Auto-correlation and ARIMA models.     

ELSSA relied on an Independent research that suggests that most people plan and book their vacations 2 months in advance, so that these advertisements will be suggested for the months 2 months prior to forecasted month. Example: January 2013 Advertisements will be run keeping in mind the March 2013 predictions. If predicted March visitor inflow is 20% of predicted annual inflow, then 20% marketing budget will be suggested to be allocated in January advertising campaigns.    

Using multiple time series analytical techniques, we will suggest which category of NZ Tourism advertisement could be allocated how much budget to run for each month of the year 2013 and with how much allocation for different genres of advertisements whether Business, Education or Vacation (depending on predicted visitor inflow for that month as a percentage of the year).   


# Conclusion:

It is deducible that the months of December and January have maximum visitor inflow, possibly because it is summer time in New Zealand when it is winter time in other countries. However, this is mostly owing to the fact that tourism and vacationing visitors make up the bulk of the overall inflow.  

It is noteworthy that proportionately, educational and business-oriented traffic peaks in the middle of the year when vacationers dip relatively. This is indicated by the June 2013 traffic forecast where Corporate and Educational visitors constitute 22.55% of the traffic and Vacation visitors are at their year minimum of 77.46%. This is also one of the least busy months in the year for New Zealand and is exact opposite of countries like USA, in the northern hemisphere.   



