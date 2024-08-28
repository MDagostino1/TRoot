# CROOTBOX PARAMETERS

# We create variables that will contain the 
# results of our crootbox simulations
all_rootsystems <- NULL
all_rlds <- NULL
n_cr_sim <- 1


########################################################################
# 3 : RUN CROOTBOX
########################################################################

# Output the advancement in the simulation
n_cr_sim <- n_cr_sim + 1
#print(paste0(n_cr_sim, " / ", n_tot_simulation, " crootbox sims"))

# Run crootbox

system("inputs/crootbox.exe") # Run crootbox for windows
file.info("outputs/current_rootsystem.txt")$mtime


# Load the simulated data into R to process it and to store it for further use
rootsystem <- fread("outputs/current_rootsystem.txt", header = T)
current_rootsystem = rootsystem

# We enrich the root system simulation data with metadata
# This is needed to find back the information in the large data file 
# at the end of the simulations
#current_rootsystem <- current_rootsystem %>% 
#  mutate(simulation_id = n_cr_sim)

# We store the root system simulation with a unique name
st <- Sys.time()
st <- as.numeric(st)
write_csv(current_rootsystem, 
          paste0("outputs/rootsystems/rootsystem_",st,".csv"))  


# write_csv(current_rootsystem, 
# paste0("outputs/rootsystems/rootsystem_charle.csv"))  

# OPTIONAL
# extract the root length density from the simulation data
# The idead here is, if needed, to store only the relevant info
# out of our simulation run and discard the rest

rld <- data.frame(simulation_id = n_cr_sim, 
                  total_length = sum(current_rootsystem$length), 
                  n_root = nrow(current_rootsystem))

# We store in a data frame the rld from all the simulations
all_rlds <- rbind(all_rlds, rld)
write_csv(all_rlds, "outputs/all_rld_crootbox.csv")





# PLOT
is_true = F

if(is_true == T){
  current_rootsystem %>%
    ggplot() +
    theme_classic() +
    geom_segment(aes(x = x1, y = z1, xend = x2, yend = z2), alpha=0.9) +
    coord_fixed()
}





# Import Online
#rootsystem <- fread("outputs/1.txt", header = T)
#current_rootsystem <- rootsystem
#current_rootsystem %>%
#  ggplot() +
#  theme_classic() +
#  geom_segment(aes(x = x1, y = z1, xend = x2, yend = z2), alpha=0.9) +
#  coord_fixed()
