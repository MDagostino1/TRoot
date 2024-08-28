
# The code should compile with any c++11 compiler, e.g. for g++: MinGW has been tested. Then create a new system environment variable. Path --> C:\MinGW\bin
# 
# Open the terminal:
#   cd ~/GitHub/marshal-pipeline/17_06 CRootBox
# 
# FOR MAC AND LINUX
# g++ *.cpp -std=c++11 -o crootbox.out   
# crootbox.out   
#
#
# FOR WINDOWS
# g++ *.cpp -std=c++11 -o crootbox.exe   
# crootbox.exe

#setwd("/Users/g.lobet/Dropbox/science/teaching/UCL/LBRAI2219/2020/crootbox-marshal")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))      

########################################################################
# 1 : LOAD THE LIBRARIES AND SOURCE FILES
########################################################################

library(tidyverse)
library(plyr)
library(readr)
library(data.table)
library(dplyr)
library(Matrix)

# Custom functions
source("inputs/io_function.R") # CROOTBOX
source("inputs/getSUF.R") # MARSHAL

# Update the crootbox executable file
# MAC
#file.copy("inputs/crootbox_source/crootbox.out", "inputs/crootbox.out", overwrite = T)
# WINDOWS
file.copy("inputs/crootbox_source/crootbox.exe","inputs/crootbox.exe", overwrite = T)

########################################################################
# 2 : SET THE SIMULATION PARAMETERS
########################################################################

# CROOTBOX PARAMETERS

# A. We store the input parameters we want to change in vectors, so 
# we can loop on these afterwards
#vitesse_primaire_vec <- c(4)
#vitesse_secondaire_vec <- c(4)
#simulation_time <- 30

# B. We load the default parameter sets for the simulation 
rparam <- read_rparam(path = "inputs/param.rparam")
pparam <- read_pparam(path = "inputs/param.pparam")

# C. We create variables that will contain the 
# results of our crootbox simulations
all_rootsystems <- NULL
all_rlds <- NULL
n_tot_simulation <- length(vitesse_primaire_vec) * length(vitesse_secondaire_vec)
n_cr_sim <- 0



# MARSHAL PARAMETERS

# D. We store the input parameters we want to change in vectors, so 
# we can loop on these afterwards
kx_vec <-c(0.1,1,10)
kr_vec <- c(0.1,1,10)

# E. We load the default parameter sets for the simulation 
psiCollar <- -15000
soil <- read_csv("inputs/soil.csv")
conductivities <- read_csv("inputs/conductivities.csv")

# F. We create variables that will contain the 
# results of our marshal simulations
all_marshal <- NULL
n_marshal_sim <- 0
tot_marshal_sim <- length(kr_vec) * length(kx_vec)


########################################################################
# 3 : RUN CROOTBOX
########################################################################

# We loop over the input parameters vectors (see 2.A)
for(vp in vitesse_primaire_vec){
  for(vs in vitesse_secondaire_vec){
  
    # Output the advancement in the simulation
    n_cr_sim <- n_cr_sim + 1
    print(paste0(n_cr_sim, " / ", n_tot_simulation, " crootbox sims"))
    
    
    
    # Modify parmeters
    
  
    # update "vitesse croissance primaire"
    rparam$val1[rparam$name == "Taproot" & rparam$param == "r"] <- vp 
    
    # update "vitesse croissance primaire"
    rparam$val1[rparam$name == "Taproot" & rparam$param == "r"] <- vp 
    
    # update the simulation time
    pparam$val1[pparam$param == "simtime"] <- simulation_time
  
    # update the input text files 
    write_rparam(rparam, "inputs/param.rparam")
    write_pparam(pparam, "inputs/param.pparam")
    
    
    
    # Run crootbox
    system("inputs/crootbox.out") # Run crootbox for mac and linux
    # system("inputs/crootbox.exe") # Run crootbox for windows
    
    
    
    # Load the simulated data into R to process it and to store it for further use
    current_rootsystem <- fread("outputs/current_rootsystem.txt", header = T)
    
    # We enrich the root system simulation data with metadata
    # This is needed to find back the information in the large data file 
    # at the end of the simulations
    current_rootsystem <- current_rootsystem %>% 
      mutate(vitesse_primaire = vp, 
             vitesse_secondaire = vs,
             simulation_id = n_cr_sim)
    
    # We store the root system simulation with a unique name
    write_csv(current_rootsystem, 
              paste0("outputs/rootsystems/rootsystem_",vp,"_",vs,".csv"))  
    
    
    # write_csv(current_rootsystem, 
              # paste0("outputs/rootsystems/rootsystem_charle.csv"))  
    
    # OPTIONAL
    # extract the root length density from the simulation data
    # The idead here is, if needed, to store only the relevant info
    # out of our simulation run and discard the rest
    
    rld <- data.frame(vitesse_primaire = vp, 
                      vitesse_secondaire = vs,
                      simulation_id = n_cr_sim, 
                      total_length = sum(current_rootsystem$length), 
                      n_root = nrow(current_rootsystem))
             
    # We store in a data frame the rld from all the simulations
    all_rlds <- rbind(all_rlds, rld)
  } 
  write_csv(all_rlds, "outputs/all_rld_crootbox.csv")
}
write_csv(all_rlds, "outputs/all_rld_crootbox.csv")






########################################################################
# 4 : RUN MARSHAL
########################################################################
# We loop over the input parameters vectors (see 2.D)

# Load the root system data you would like to use with marshal. 
# It is also possible to loop over a list of root systems is needed
rootsystem <- fread("outputs/rootsystems/rootsystem_3_0.7.csv", 
                    header = T) %>% 
  select(-c(vitesse_primaire, vitesse_secondaire, simulation_id))

#rootsystem <- fread("outputs/rootsystems/rootsystem_charle.csv", header = T)

for(kr_in in kr_vec){
  for(kx_in in kx_vec){
    
      n_marshal_sim <- n_marshal_sim + 1
      print(paste0(n_marshal_sim, " / ", tot_marshal_sim, " marshal sims"))
      
      # Change the value of the conductivities according to
      # the values in the kx_vec and kr_vec. 
      # Here the values in kx_vec and kr_vec are modifiers, not absolute values
      conds <- conductivities %>% 
            mutate(y = ifelse(type == "kr", y * kr_in, y * kx_in))
      
        
      # Run MARSHAL
      hydraulics <- getSUF(rootsystem, conds, soil, psiCollar)
      
      hydraulic_archi <- hydraulics$root_system
      hydraulic_archi$suf <- hydraulics$suf[,1]
      hydraulic_archi$kr <- hydraulics$kr[,1]
      hydraulic_archi$kx <- hydraulics$kx[,1]
      hydraulic_archi$jr <- hydraulics$jr[,1]
      hydraulic_archi$jxl <- hydraulics$jxl[,1]
      hydraulic_archi$psi <- hydraulics$psi[,1]
      
      
        
      # Save all the results of the simulation
      save(hydraulic_archi, file = paste0("outputs/marshal/hydraulics_",kx_in,"_",kr_in,".RData"))
      
      # Save only the relevant results of the simulation data in a new dataframe. 
      all_marshal <- rbind(all_marshal, 
                           tibble(
                             "kx" = kx_in,
                             "kr" = kr_in,
                             "length" = sum(rootsystem$length), 
                             "transpiration" = hydraulics$tact, 
                             "krs" = hydraulics$krs)
      )
  }
  write_csv(all_marshal, "outputs/all_marshal.csv")
}
write_csv(all_marshal, "outputs/all_marshal.csv")





########################################################################
# 5 : PLOT THE RESULTS
########################################################################
source("plots.R")

########################################################################
# 6 : geSUF
########################################################################
source("getSUF.R")
SUF <- getSUF(rootsystem,conductivities,soil)

# Integration sur 20 pas :
SUF_init <- SUF$suf
SUF_clean <- c()
step <- as.integer(length(SUF$suf)/20)
start <- 1
stop <- step
for (i in c(1:20)){
  #print(paste0("start : ",start," stop : ",stop))
  SUF_clean <- c(SUF_clean, sum(SUF_init[start:stop]))
  start <- start+step
  stop <- stop+step
}












