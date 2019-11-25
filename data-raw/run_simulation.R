library(apsimx)

#Use the apsimx package to run apsim next generation
simulation <- apsimx(file = "special_project_model.apsimx", 
              src.dir = "./data-raw/archive", 
              silent = TRUE, value = 'report', cleanup = TRUE)

#Partition datasets (keep secret unused until final evaluation at end of the semester)
training <- simulation %>% filter(year(Date) < 2005) %>%
  mutate(year = year(Date), yday = yday(Date))
testing <- simulation %>% filter(year(Date) >= 2005 & year(Date) < 2012) %>%
  mutate(year = year(Date), yday = yday(Date))
secret <- simulation %>% filter(year(Date) >= 2012) %>%
  mutate(year = year(Date), yday = yday(Date))

#Save RData to the data directory
usethis::use_data(simulation, overwrite=TRUE)
usethis::use_data(training, overwrite=TRUE)
usethis::use_data(testing, overwrite=TRUE)
usethis::use_data(secret, overwrite=TRUE)