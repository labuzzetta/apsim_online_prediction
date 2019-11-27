#Load required libraries
library(apsimx)
library(ggplot2)
library(lubridate)
library(dplyr)
library(mgcv)
library(purrr)

#Load datasets from package
data("training")
data("testing")

#Add daily biomass change variable - training
trn <- training %>% arrange(Date) %>%
  mutate(Maize.Wt.Change = Maize.AboveGround.Wt - lag(Maize.AboveGround.Wt))
trn[1, "Maize.Wt.Change"] <- 0

#Add daily biomass change variable - testing
tst <- testing %>% arrange(Date) %>%
  mutate(Maize.Wt.Change = Maize.AboveGround.Wt - lag(Maize.AboveGround.Wt))
tst[1, "Maize.Wt.Change"] <- 0

combo <- rbind(trn, tst)
testing_index <- which(year(combo$Date) >= 2005 & year(combo$Date) < 2012)

#RF Method
#rforest <- randomForest::randomForest(data = trn,
#                                      Maize.Wt.Change ~ Weather.Rain + Weather.Radn + 
#                                        Weather.MaxT + Weather.MeanT + Weather.MinT +
#                                        Weather.VPD + yday + year)

#Model residuals via online prediction method by updating RF model
#with newly observed data after each iteration
rf.forecast <- numeric(length(testing_index))
harvested = FALSE
year.tot = 0
for (i in seq(nrow(tst))) {
  
  #Set default forecast value of 0
  forecast <- 0
  
  #Get day of the year (May 5th is either 125 or 126)
  dayofyear <- yday(tst$Date[i])
  
  #Has the seed been sown?
  sowed = dayofyear >= 125
  
  #Reset the harvested boolean when the crop is sown
  if(dayofyear == 125){
    harvested = FALSE
  }
  
  #If the crop is sown and not yet harvested
  if(sowed && !harvested){
    
    new_rf <- gam(data = combo[testing_index[i-30]:testing_index[i-1],],
                  Maize.Wt.Change ~ Weather.Rain + Weather.Radn + 
                    Weather.MaxT + Weather.MeanT + Weather.MinT +
                    Weather.VPD + s(yday) + year)
    
    #If the difference between the previous prediction and the next is more than 500,
    #then the model has predicted that the crop was harvested, else predict
    if(as.numeric(predict(new_rf, tst[i,])) < -50){
      #If crop was harvested, return Maize.AboveGround.Wt forecast of 0
      harvested = TRUE
      forecast = -1 * year.tot
    } else {
      #If the crop was not harvested, return the AR model online prediction
      forecast <- as.numeric(predict(new_rf, tst[i,]))
      #Do not return a negative forecast
      forecast <- max(0, forecast)
    }
  }
  
  #Update forecasts list
  rf.forecast[i] <- forecast
  year.tot <- year.tot + forecast
}

sqrt(mean((rf.forecast - tst$Maize.Wt.Change)^2))
plot(rf.forecast %>% purrr::accumulate(`+`))
lines(tst$Maize.AboveGround.Wt)