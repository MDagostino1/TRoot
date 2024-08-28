DIR_path <- getwd()
setwd(dir = DIR_path)

JOB_NUMBER <- 1
GRANAR2_path <- "GRANAR/R/"
source("GRANAR/R/load_packages.R")
for (i in list.files(GRANAR2_path, pattern=".R")) {
  # print(i)
  source(paste0(GRANAR2_path, i))
}

file_list <- list.files("Data/VR_loop/", pattern = ".csv")
VR_temp <- read.csv(paste0("Data/VR_loop/", file_list[1]))
print("Loading data successfull.")

use_condaenv("MECHA2")
# use_condaenv("MECHA2", conda = "C:/Users/m.dagostino/AppData/Local/anaconda3/envs/MECHA2") # choose MECHA environment

print("Launching Make_Virtual_Anatomies...")

mecha <- c("./MECHA/MECHAv4_septa_temp_V2.py")
output <- c("./MECHA/Projects/granar/out/Tomato/Root/Project_Test/results/")

AQP_list <- c(0.0001, 0.00043)

VR_temp <- VR_temp %>% filter(x == 0 | x == 1) # just for testing 2 anatomies

Make_Virtual_Anatomies_M(Virtual_Roots = VR_temp,
                         MECHA_path = mecha,
                         output_path = output,
                         AQP_list = AQP_list)



