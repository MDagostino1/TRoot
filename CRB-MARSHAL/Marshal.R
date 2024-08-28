########################################################################
# 4 : RUN MARSHAL
########################################################################
    
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
                       "krs" = hydraulics$krs))
                         
    
    
  


