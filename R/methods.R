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

#AR Method General Stub


#RF Method General Stub


