#Load required libraries
library(apsimx)
library(ggplot2)
library(lubridate)
library(dplyr)
library(mgcv)

data("simulation")
report <- simulation

#Training Data Yield Figure
ggplot(data = report %>%
         filter(year(Date) < 2005) %>%
         mutate(Year = as.factor(lubridate::year(Date)),
                `Above Ground Biomass (g/m^2)` = Maize.AboveGround.Wt,
                `Day of Year` = lubridate::yday(Date)),
       aes(x = `Day of Year`, y = `Above Ground Biomass (g/m^2)`, col = Year)) + 
  geom_line()

#Testing and Validation Data Yield Figure
ggplot(data = report %>%
         filter(year(Date) >= 2005) %>%
         mutate(Year = as.factor(lubridate::year(Date)),
                `Above Ground Biomass (g/m^2)` = Maize.AboveGround.Wt,
                `Day of Year` = lubridate::yday(Date)),
       aes(x = `Day of Year`, y = `Above Ground Biomass (g/m^2)`, col = Year)) + 
  geom_line()

#Partition datasets (keep secret unused until final evaluation at end of the semester)
training <- report %>% filter(year(Date) < 2005) %>%
  mutate(year = year(Date), yday = yday(Date))
testing <- report %>% filter(year(Date) >= 2005 & year(Date) < 2012) %>%
  mutate(year = year(Date), yday = yday(Date))
testing_index <- which(year(report$Date) >= 2005 & year(report$Date) < 2012)
secret <- report %>% filter(year(Date) >= 2012) %>%
  mutate(year = year(Date), yday = yday(Date))

#Save report data without secret partition
report <- rbind(training, testing)

#Adapted from https://rdrr.io/cran/opera/man/opera-package.html#heading-3
#Fit gam model with relevant explanatory variables
detrend.fit <- gam(Maize.AboveGround.Wt ~ Weather.Rain + Weather.Radn + 
                     Weather.MaxT + Weather.MeanT + Weather.MinT +
                     Weather.VPD + s(yday), data = training)

#Use model to make generalized prediction on future meteorological variables
report$Trend <- c(predict(detrend.fit), predict(detrend.fit,newdata = testing))
#Detrend the predictions, essentially calculating residuals
report$Maize.AboveGround.Wt.detrend <- report$Maize.AboveGround.Wt - report$Trend

#Model residuals via online prediction method by updating AR model
#with newly observed data after each iteration
ar.forecast <- numeric(length(testing_index))
harvested = FALSE
for (i in seq(testing_index)) {
  
  #Fit AR model on previously observed residuals
  ar.fit <- ar(report$Maize.AboveGround.Wt.detrend[1:(testing_index[i] - 1)])
  
  #Set default forecast value of 0
  forecast <- 0
  
  #Get day of the year (May 5th is either 125 or 126)
  dayofyear <- yday(testing$Date[i])
  
  #Has the seed been sown?
  sowed = dayofyear >= 125
  
  #Reset the harvested boolean when the crop is sown
  if(dayofyear == 125){
    harvested = FALSE
  }
  
  #If the crop is sown and not yet harvested
  if(sowed && !harvested){
    
    #If the previous APSIM output says the corn is ripe, then harvest
    #This will allow for better comparison between the methods in terms of RMSE
    #And none of the methods I have used predict harvesting well.
    if(tst[i-1,]$Maize.Phenology.CurrentStageName == "HarvestRipe"){
      #If crop was harvested, return Maize.AboveGround.Wt forecast of 0
      harvested = TRUE
    } else {
      #If the crop was not harvested, return the AR model online prediction
      forecast <- as.numeric(predict(ar.fit)$pred) + report$Trend[testing_index[i]]
      #Do not return a negative forecast
      forecast <- max(0, forecast)
    }
  }
  
  #Update forecasts list
  ar.forecast[i] <- forecast
}


sqrt(mean((ar.forecast - testing$Maize.AboveGround.Wt)^2))
plot(ar.forecast)
lines(ar.forecast)

