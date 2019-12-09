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

combotest <- combo; 
combotest[which(combotest$Maize.Phenology.CurrentStageName == ""),]$Maize.Phenology.CurrentStageName <- NA;
combotest[which(combotest$Maize.Phenology.CurrentStageName == "HarvestRipe"),]$Maize.Phenology.CurrentStageName <- "Ripe"
combotest[which(combotest$Maize.Phenology.CurrentStageName == "Ripe")+1,]$Maize.Phenology.CurrentStageName <- "Harvest"
combotest[which(combotest$yday == 1),]$Maize.Phenology.CurrentStageName <- "StartYear"; 
combo <- combotest %>% tidyr::fill("Maize.Phenology.CurrentStageName")

combo$growing <- dplyr::if_else(condition = combo$Maize.Phenology.CurrentStageName != "StartYear" &
                                            combo$Maize.Phenology.CurrentStageName != "Harvest", 
                         1, 0)

newtrn <- combo[which(year(combo$Date) < 2005),]
newtst <- combo[which(year(combo$Date) >= 2005),]





#####################Neural Net Soil#######################

caretnnet <- caret::train(data = trn, method = "nnet",
                         Soil.SoilWater.Runoff ~ Weather.Rain + Weather.Radn + 
                           Weather.MaxT + Weather.MeanT + Weather.MinT +
                           Weather.VPD + yday + year, linout = 1)

plot(tst$Soil.SoilWater.Runoff)
points(as.numeric(predict(caretnnet, tst)), col = "red")
sqrt(mean((as.numeric(predict(caretnnet, tst)) - tst$Soil.SoilWater.Runoff)^2))

caretrf <- randomForest::randomForest(data = trn, ntrees = 1000, 
                        Soil.SoilWater.Runoff ~ Weather.Rain + Weather.Radn +
                          Weather.MaxT + Weather.MeanT + Weather.MinT +
                          Weather.VPD + yday + year)

plot(tst$Soil.SoilWater.Runoff)
points(as.numeric(predict(caretrf, tst)), col = "red")
sqrt(mean((as.numeric(predict(caretrf, tst)) - tst$Soil.SoilWater.Runoff)^2))
