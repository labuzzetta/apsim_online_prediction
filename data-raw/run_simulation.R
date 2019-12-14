library(apsimx)
library(dplyr)

#Use the apsimx package to run apsim next generation
simulation <- apsimx(file = "special_project_model.apsimx", 
              src.dir = "./data-raw/archive", 
              silent = TRUE, value = 'report', cleanup = TRUE)

simulation <- simulation %>% mutate(year = year(Date), yday = yday(Date))

simulation[which(simulation$Maize.Phenology.CurrentStageName == ""),]$Maize.Phenology.CurrentStageName <- NA;
simulation[which(simulation$Maize.Phenology.CurrentStageName == "HarvestRipe"),]$Maize.Phenology.CurrentStageName <- "Ripe"
simulation[which(simulation$Maize.Phenology.CurrentStageName == "Ripe")+1,]$Maize.Phenology.CurrentStageName <- "Harvest"
simulation[which(simulation$yday == 1),]$Maize.Phenology.CurrentStageName <- "StartYear"; 
simulation <- simulation %>% tidyr::fill("Maize.Phenology.CurrentStageName")

simulation$growing <- dplyr::if_else(condition = simulation$Maize.Phenology.CurrentStageName != "StartYear" &
                                       simulation$Maize.Phenology.CurrentStageName != "Harvest", 
                                     1, 0)

#Partition datasets (keep secret unused until final evaluation at end of the semester)
training <- simulation %>% filter(year(Date) < 2005)
testing <- simulation %>% filter(year(Date) >= 2005 & year(Date) < 2012)
secret <- simulation %>% filter(year(Date) >= 2012)

#Save RData to the data directory
usethis::use_data(simulation, overwrite=TRUE)
usethis::use_data(training, overwrite=TRUE)
usethis::use_data(testing, overwrite=TRUE)
usethis::use_data(secret, overwrite=TRUE)