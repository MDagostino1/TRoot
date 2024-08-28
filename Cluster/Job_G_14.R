DIR_path <- getwd()
    setwd(dir = DIR_path)

    JOB_NUMBER <- 14
    GRANAR2_path <- "GRANAR/R/"
    source("GRANAR/R/load_packages.R")
    library(rgeos)
    for (i in list.files(GRANAR2_path, pattern=".R")) {
      # print(i)
      source(paste0(GRANAR2_path, i))
    }

    file_list <- list.files("Data/VR_loop/", pattern = ".csv")
    VR_temp <- read.csv(paste0("Data/VR_loop/", file_list[14]))
    print("Loading data successfull.")

    print("Launching Make_Virtual_Anatomies...")

    Make_Virtual_Anatomies_G(VR_temp)
    
