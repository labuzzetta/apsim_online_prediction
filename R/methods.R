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

#AR Method
detrend.fit <- gam(Maize.Wt.Change ~ s(Weather.Rain) + s(Weather.Radn) + 
                     s(Weather.MaxT) + s(Weather.MeanT) + s(Weather.MinT) +
                     s(Weather.VPD) + s(yday) + as.factor(year), data = trn)

combo <- rbind(trn, tst)
testing_index <- which(year(combo$Date) >= 2005 & year(combo$Date) < 2012)

#Use model to make generalized prediction on future meteorological variables
combo$Trend <- c(predict(detrend.fit), predict(detrend.fit,newdata = tst))
#Detrend the predictions, essentially calculating residuals
combo$Maize.Wt.Change.detrend <- combo$Maize.Wt.Change - combo$Trend

#Model residuals via online prediction method by updating AR model
#with newly observed data after each iteration
ar.forecast <- numeric(length(testing_index))
harvested = FALSE
year.tot = 0
for (i in seq(testing_index)) {
  
  #Fit AR model on previously observed residuals
  ar.fit <- ar(combo$Maize.Wt.Change.detrend[1:(testing_index[i] - 1)])
  
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
    
    #If the difference between the previous prediction and the next is more than 500,
    #then the model has predicted that the crop was harvested, else predict
    if((as.numeric(predict(ar.fit)$pred) + combo$Trend[testing_index[i]]) < -10){
      #If crop was harvested, return Maize.AboveGround.Wt forecast of 0
      harvested = TRUE
      forecast = -1 * year.tot
    } else {
      #If the crop was not harvested, return the AR model online prediction
      forecast <- as.numeric(predict(ar.fit)$pred) + combo$Trend[testing_index[i]]
      #Do not return a negative forecast
      forecast <- max(0, forecast)
    }
  }
  
  #Update forecasts list
  ar.forecast[i] <- forecast
  year.tot <- year.tot + forecast
}

#Total RMSE
sqrt(mean((ar.forecast - tst$Maize.Wt.Change)^2))
plot(ar.forecast %>% purrr::accumulate(`+`))
lines(tst$Maize.AboveGround.Wt)

#RF Method
rforest <- randomForest::randomForest(data = trn,
                                      Maize.Wt.Change ~ Weather.Rain + Weather.Radn + 
                                        Weather.MaxT + Weather.MeanT + Weather.MinT +
                                        Weather.VPD + yday)

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
    
    #If the difference between the previous prediction and the next is more than 500,
    #then the model has predicted that the crop was harvested, else predict
    if(as.numeric(predict(rforest, tst[i,])) < -50){
      #If crop was harvested, return Maize.AboveGround.Wt forecast of 0
      harvested = TRUE
      forecast = -1 * year.tot
    } else {
      #If the crop was not harvested, return the AR model online prediction
      forecast <- as.numeric(predict(rforest, tst[i,]))
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

#GBM Method
gbm <- caret::train(data = trn,
                    Maize.Wt.Change ~ Weather.Rain + Weather.Radn + 
                      Weather.MaxT + Weather.MeanT + Weather.MinT +
                      Weather.VPD + yday, method = "gbm")

#Model residuals via online prediction method by updating GBM model
#with newly observed data after each iteration
gbm.forecast <- numeric(length(testing_index))
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
    
    #If the difference between the previous prediction and the next is more than 500,
    #then the model has predicted that the crop was harvested, else predict
    if(as.numeric(predict(gbm, tst[i,])) < -1){
      #If crop was harvested, return Maize.AboveGround.Wt forecast of 0
      harvested = TRUE
      forecast = -1 * year.tot
    } else {
      #If the crop was not harvested, return the AR model online prediction
      forecast <- as.numeric(predict(gbm, tst[i,]))
      #Do not return a negative forecast
      forecast <- max(0, forecast)
    }
  }
  
  #Update forecasts list
  gbm.forecast[i] <- forecast
  year.tot <- year.tot + forecast
}

sqrt(mean((gbm.forecast - tst$Maize.Wt.Change)^2))
plot(gbm.forecast %>% purrr::accumulate(`+`))
lines(tst$Maize.AboveGround.Wt)
