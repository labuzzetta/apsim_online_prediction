#' Perform online prediction for Maize outputs from APSIMX
#' 
#' This function performs online prediction on next generation
#' APSIM data. The 'train' data should be in the same format as
#' the 'test' data and include the standard weather varibles. 
#' Currently, this function works best for Maize model outputs,
#' but may be applicable to other outputs. Variables with
#' non-zero outputs during days of the year when the crop is 
#' not growing will be predicted to be 0 and likely innaccurate.
#' 
#' \code{train} A data.frame of training data
#' 
#' \code{test} A data.frame of data to perform online prediction
#' 
#' \code{pred_var} Any Maize model APSIM output variable
#' 
#' \code{pred_type} One of "no_update", "online_total_local", 
#' "online_total_full", "online_change_local", "online_change_full"
#' 
#' \code{method} One of "lm", "ar", "gam", "rf", "nnet"
#' 
#' \code{local_dist} Number of sliding window observations for local training
#' 
#' @export
#' @import apsimx
#' @import ggplot2
#' @import dplyr
#' @importFrom lubridate yday
#' @import mgcv
#' @import tidyr
#' @import caret
#' 
#' @examples
#' data("training")
#' data("testing")
#' on <- online_emulate_maize(training, testing, pred_var = "Maize.AboveGround.Wt", pred_type = "online_total_local", method = "gam")
#' plot(testing$Date, on, type = "l")

online_emulate_maize <- function(train, test, pred_var, pred_type, method, local_dist = 30){
  
  #List of required variables for online prediction
  required <- c("Weather.Rain", "Weather.Radn", "Weather.MaxT", "Weather.MeanT", 
                "Weather.MinT", "Weather.VPD", "Date", "yday", "year",
                "Maize.Phenology.CurrentStageName")
  
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
  if(pred_type %in% c("online_change_full", "online_change_local")){
    #Add daily change variable in train data
    train <- train %>% arrange(Date) %>%
      mutate(Change = .data[[pred_var]] - lag(.data[[pred_var]]))
    train[1, "Change"] <- 0
    
    #Add daily change variable in test data
    test <- test %>% arrange(Date) %>%
      mutate(Change = .data[[pred_var]] - lag(.data[[pred_var]]))
    test[1, "Change"] <- 0
    
    pred_var <- "Change"
  }
  
  #Assumes testing data comes after training data
  combo <- rbind(train, test)
  testing_index <- seq(nrow(train)+1, nrow(train)+nrow(test))
  
  model <- NULL
  
  #List of covariates
  independent <- c("Weather.Rain","Weather.Radn","Weather.MaxT","Weather.MeanT","Weather.MinT","Weather.VPD","yday","year")
  
  #Instantiate ar model components
  if(method == "ar"){
    detrend.fit <- gam(data = combo[seq(1,nrow(train)),],
                       as.formula(paste(pred_var," ~ ","Weather.Rain + Weather.Radn + Weather.MaxT + Weather.MeanT + Weather.MinT + Weather.VPD + s(yday) + year")))
    #Use model to make generalized prediction on future meteorological variables
    combo$Trend <- c(predict(detrend.fit), predict(detrend.fit, newdata = test))
    #Detrend the predictions, essentially calculating residuals
    combo$detrend <- combo[[pred_var]] - combo$Trend
  }
  
  #Train models which do not update throughout prediction phase
  if(pred_type == "no_update"){
    if(method == "rf"){
      model <- randomForest::randomForest(data = combo[seq(1,nrow(train)) %in% which(combo$growing == 1),],
                                          as.formula(paste(pred_var," ~ ",paste(independent,collapse="+"))))
    } else if(method == "nnet"){
      model <- caret::train(data = combo[seq(1,nrow(train)) %in% which(combo$growing == 1),], method = "nnet", linout = 1,
                            as.formula(paste(pred_var," ~ ",paste(independent,collapse="+"))))
    } else if(method == "gam"){
      model <- gam(data = combo[seq(1,nrow(train)) %in% which(combo$growing == 1),],
                            as.formula(paste(pred_var," ~ ","Weather.Rain + Weather.Radn + Weather.MaxT + Weather.MeanT + Weather.MinT + Weather.VPD + s(yday) + year")))
    } else if(method == "lm"){
      model <- lm(data = combo[seq(1,nrow(train)) %in% which(combo$growing == 1),],
                   as.formula(paste(pred_var," ~ ",paste(independent,collapse="+"))))
    } else {
      model <- ar(combo$detrend[seq(1,nrow(train))])
    }
  }
  
  #Model via online prediction method dependent on 
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
    if(test$Maize.Phenology.CurrentStageName[i] == "Sowing"){
      harvested = FALSE
    }
    
    #If the crop is sown and not yet harvested
    if(sowed && !harvested){
        
      tmp <- c("rf", "nnet", "ar", "gam", "lm")
      
      training_indicies <- c()
      if(pred_type %in% c("online_total_full", "online_change_full")){
        training_indicies <- seq(1,testing_index[i-1]) %in% which(combo$growing == 1)
      } else if(pred_type %in% c("online_total_local", "online_change_local")){
        training_indicies <- seq(testing_index[i-local_dist],testing_index[i-1])
      } else {
        training_indicies <- seq(1,nrow(train))
      }
  
      if(method == "rf" & (pred_type != "no_update")){
        model <- randomForest::randomForest(data = combo[training_indicies,],
                                            as.formula(paste(pred_var," ~ ",paste(independent,collapse="+"))))
      } else if(method == "nnet" & (pred_type != "no_update")){
        model <- caret::train(data = combo[training_indicies,], method = "nnet", linout = 1,
                              as.formula(paste(pred_var," ~ ",paste(independent,collapse="+"))))      
      } else if(method == "gam" & (pred_type != "no_update")){
        model <- gam(data = combo[training_indicies,],
                     as.formula(paste(pred_var," ~ ","Weather.Rain + Weather.Radn + Weather.MaxT + Weather.MeanT + Weather.MinT + Weather.VPD + s(yday) + year")))
      } else if(method == "lm" & (pred_type != "no_update")){
        model <- lm(data = combo[training_indicies,],
                    as.formula(paste(pred_var," ~ ",paste(independent,collapse="+"))))
      } else if(method == "ar" & (pred_type != "no_update")){
        model <- ar(combo$detrend[seq(1,testing_index[i-1])])
      }
      
      #If the previous APSIM output says the corn is ripe, then harvest
      #This will allow for better comparison between the methods in terms of RMSE
      #And none of the methods I have used predict harvesting well.
      if(test[i-1,]$Maize.Phenology.CurrentStageName == "Ripe"){
        #If crop was harvested, return Maize.AboveGround.Wt forecast of 0
        harvested = TRUE
        sowed = FALSE
        if(pred_type %in% c("online_change_full", "online_change_local")){
          forecast = -1 * year.tot
        } else {
          forecast = 0
        }
      } else {
        #If the crop was not harvested, return model prediction
        if(method != "ar"){
          forecast <- as.numeric(predict(model, test[i,]))
        } else {
          forecast <- as.numeric(predict(model)$pred) + combo$Trend[testing_index[i]]
        }
        #Do not return a negative forecast
        forecast <- max(0, forecast)
      }
    }
    
    #Update forecasts list
    forecasts[i] <- forecast
    if(pred_type %in% c("online_change_full", "online_change_local")){
      year.tot <- year.tot + forecast
    }
    
  }
  
  return(forecasts)
  
}