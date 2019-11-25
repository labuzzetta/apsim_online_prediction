#Load required libraries
library(apsimx)
library(ggplot2)
library(lubridate)
library(dplyr)
library(mgcv)

#Load datasets from package
data("training")
data("testing")

methods <- rbind(training, testing) %>%
  arrange(Date) %>% 
  mutate(Maize.Wt.Change = Maize.AboveGround.Wt - lag(Maize.AboveGround.Wt))