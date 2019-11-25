library(apsimx)

#Use the apsimx package to run apsim next generation
simulation <- apsimx(file = "special_project_model.apsimx", 
              src.dir = "./data-raw/archive", 
              silent = TRUE, value = 'report', cleanup = TRUE)

#Save RData to the data directory
usethis::use_data(simulation, overwrite=TRUE)