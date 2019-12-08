online_emulate_maize <- function(train, test, pred_var, pred_type, method){
  
  #Load required libraries
  library(apsimx)
  library(ggplot2)
  library(lubridate)
  library(dplyr)
  library(mgcv)
  library(tidyr)
  
  #List of required variables for online prediction
  required <- c("Weather.Rain", "Weather.Radn", "Weather.MaxT", "Weather.MeanT", 
                "Weather.MinT", "Weather.VPD", "Date", "Maize.Phenology.CurrentStageName")
  
  #Check training and testing data.frames for required variables
  if(!all(required %in% colnames(train))) stop("Check required column names in training data")
  if(!all(required %in% colnames(test))) stop("Check required column names in testing data")
  if(!(pred_var %in% colnames(train))) stop("Variable to predict not contained in training data")
  if(!(pred_var %in% colnames(test))) stop("Variable to predict not contained in testing data")
  
  #Check selected prediction type is valid
  if(!(pred_type %in% c("online_total_full", "online_change_full",
                        "online_total_local", "online_change_local", 
                        "no_update"))) stop("Invalid prediction type (pred_type)")
  
  #Check method type is supported
  if(!(method %in% c("rf", "nnet", "ar", "gam", "lm"))) stop("Invalid method type (method)")
  
  #Edit the variable type to reflect daily change 
  #for online_change prediction type
  if(pred_type == "online_change"){
    #Add daily change variable in train data
    train <- train %>% arrange(Date) %>%
      mutate(Change = pred_var - lag(pred_var))
    train[1, "Change"] <- 0
    
    #Add daily change variable in test data
    test <- test %>% arrange(Date) %>%
      mutate(Change = pred_var - lag(pred_var))
    test[1, "Change"] <- 0
  }
  
  #Assumes testing data comes after training data
  combo <- rbind(trn, tst)
  testing_index <- seq(nrow(train)+1, nrow(train)+nrow(test))
  
  #Model residuals via online prediction method dependent on 
  #pred_type and method selectionse
  forecasts <- numeric(length(test))
  sowed = FALSE
  harvested = FALSE
  year.tot = 0
  for (i in seq(nrow(test))) {
    
    #Set default forecast value of 0
    forecast <- 0
    
    #Get day of the year (May 5th is either 125 or 126)
    dayofyear <- yday(test$Date[i])
    
    #Has the seed been sown?
    if(!sowed){
      sowed = (test$Maize.Phenology.CurrentStageName[i] == "Sowing")
    }
    
    #Reset the harvested boolean when the crop is sown
    if(dayofyear == 125){
      harvested = FALSE
    }
    
    #If the crop is sown and not yet harvested
    if(sowed && !harvested){
      
      new_rf <- randomForest::randomForest(data = combo[testing_index[i-30]:testing_index[i-1],],
                                           Maize.Wt.Change ~ Weather.Rain + Weather.Radn + 
                                             Weather.MaxT + Weather.MeanT + Weather.MinT +
                                             Weather.VPD + yday + year)
      
      #If the previous APSIM output says the corn is ripe, then harvest
      #This will allow for better comparison between the methods in terms of RMSE
      #And none of the methods I have used predict harvesting well.
      if(test[i-1,]$Maize.Phenology.CurrentStageName == "HarvestRipe"){
        #If crop was harvested, return Maize.AboveGround.Wt forecast of 0
        harvested = TRUE
        sowed = FALSE
        forecast = -1 * year.tot
      } else {
        #If the crop was not harvested, return the RF model online prediction
        forecast <- as.numeric(predict(new_rf, test[i,]))
        #Do not return a negative forecast
        forecast <- max(0, forecast)
      }
    }
    
    #Update forecasts list
    forecasts[i] <- forecast
    year.tot <- year.tot + forecast
  }
  
  return(forecasts)
}