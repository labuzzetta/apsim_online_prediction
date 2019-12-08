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
  if(!(pred_type %in% c("online_accumulate", "online_change", "no_update"))) stop("Invalid prediction type (pred_type)")
  
  #Check method type is supported
  if(!(method %in% c("rf", "nnet", "ar", "gam", "lm"))) stop("Invalid method type (method)")
  
  
  
}