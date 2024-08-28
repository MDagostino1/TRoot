rm(list=ls())                                                                   # Clean all variables
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#/!\/!\/!\/!\
# Entrer ici le directory du dossier Code Clean :
Path = "C:/Users/dagos/Desktop/Unif/2020-2021/Q2/LBRAI2219/Code Clean/"
# IMPORTANT : METTRE LE BON DIRECTORY DANS LE FICHIER LEVEL_01.DIR
# ex : C:\Users\dagos\Desktop\Unif\2020-2021\Q2\LBRAI2219\Hydrus_1D_Couvreur\Hydrus_1D_Couvreur - Copie\Valence
#/!\/!\/!\/!\


########################################################################
# 1 : LOAD THE LIBRARIES AND SOURCE FILES
########################################################################

Hydrus_Path = paste0(Path,"Hydrus_1D_Couvreur - Copie/")
Main_Path = paste0(Path,"crootbox-marshal")

library(tidyverse)
library(plyr)
library(readr)
library(data.table)
library(dplyr)
library(Matrix)
library(ggplot2)
library(gridExtra)

# Custom functions
source("inputs/io_function.R") # CROOTBOX
source("inputs/getSUF.R") # MARSHAL
source("SUF.R")
source("Hydrus.R")


########################################################################
# 2 : PARAMETER AND RUN CROOTBOX
########################################################################

# rparam et pparam sont préalablement complétés avec les mesures des photos.
source("CRootBox.R")

# LOAD ROOT SYSTEM (si problemes avec CRootBox ; charge des architectures generees par CRootBox online)
rootsystem <- fread("outputs/50-1.txt", header = T)
current_rootsystem <- rootsystem

# Plot root system
current_rootsystem %>% ggplot() +  theme_classic() +  geom_segment(aes(x = x1, y = z1, xend = x2, yend = z2), alpha=0.9) + coord_fixed()

########################################################################
# 3 : PARAMETER AND RUN MARSHAL + HYDRUS
########################################################################

# MARSHAL PARAMETERS
#kx_vec <-c(0.01,0.1,0.5,1,2,10)
#kr_vec <- c(0.1,0.5,1,2,10)

# Best Values
kx_vec = 0.5
kr_vec = 10

# DEFAULT PARAMETERS & VARIABLES
psiCollar <- -15000
soil <- read_csv("inputs/soil.csv")
conductivities <- read_csv("inputs/conductivities.csv")
all_marshal <- NULL
n_marshal_sim <- 0
tot_marshal_sim <- length(kr_vec) * length(kx_vec)
stock1 <- list()   # va stocker tous les outputs
stock2 <- list()
plot_list <- list() # stocke les plots
RMS_df <- data.frame(kr = c(), kx = c(), krs = c(), RMS = c())

# RUN MARSHAL & HYDRUS
for(kr_in in kr_vec){
  for(kx_in in kx_vec){
    
    n_marshal_sim <- n_marshal_sim + 1
    print(paste0(n_marshal_sim, " / ", tot_marshal_sim, " marshal sims"))
    
    # Change the value of the conductivities according to
    # the values in the kx_vec and kr_vec. 
    # Here the values in kx_vec and kr_vec are modifiers, not absolute values
    conds <- conductivities %>% 
      mutate(y = ifelse(type == "kr", y * kr_in, y * kx_in))
    
    #===========================================================================
    # Run MARSHAL
    hydraulics <- getSUF(current_rootsystem, conds, soil, psiCollar)
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
    all_marshal <- rbind(all_marshal, tibble("kx" = kx_in,
                                             "kr" = kr_in,
                                             "length" = sum(rootsystem$length),
                                             "transpiration" = hydraulics$tact,
                                             "krs" = hydraulics$krs))
    #===========================================================================
    # Calcul SUF, Krs et Kcomp
    SUF <- Calcul_SUF(hydraulics)
    Krs <- hydraulics$krs
    Krs = Krs/(75*15)
    Kcomp = Krs
    sim_name <- paste0("Kr = x",kr_in," | Kx = x",kx_in," | Krs = ",round(Krs,digits = 7))
    #===========================================================================
    # VALENCE
    
    outputs_temp1 <- Hydrus_valence(SUF,Krs,Kcomp)
    # Save 
    stock_list1 <- list(list(sim_name,outputs_temp1))
    stock1 <- c(stock1,stock_list1)
    
    # Save Plot
    data1 <- stock_list1[[1]][[2]]
    plot1 = ggplot(data1, aes(Time, y = RootWaterUptake, color = Legend)) + 
      geom_line(aes(y = vRoot, col = "vRoot [cm^3/j]")) + 
      geom_line(aes(y = rRoot, col = "rRoot [cm^3/j]")) +  
      ggtitle(paste0("Valence, ",stock_list1[[1]][[1]])) + 
      theme_set(theme_bw())
    plot1
    plot1 = list(plot1)
    ggsave(paste0("Valence_",kr_in,"_",kx_in,".png"))
    plot_list = c(plot_list,plot1)
    #=================================================================================
    # TOULOUSE
    
    outputs_temp2 <- Hydrus_toulouse(SUF,Krs,Kcomp)
    # Save 
    stock_list2 <- list(list(sim_name,outputs_temp2))
    stock2 <- c(stock1,stock_list2)
    
    # Save Plot
    data2 <- stock_list2[[1]][[2]]
    plot2 = ggplot(data2, aes(Time, y = RootWaterUptake, color = Legend)) + 
      geom_line(aes(y = vRoot, col = "vRoot [cm^3/j]")) + 
      geom_line(aes(y = rRoot, col = "rRoot [cm^3/j]")) +  
      ggtitle(paste0("Toulouse, ",stock_list2[[1]][[1]])) + 
      theme(legend.position = "top") +
      theme_set(theme_bw())
    plot2
    plot2 = list(plot2)
    ggsave(paste0("Toulouse_",kr_in,"_",kx_in,".png"))
    plot_list = c(plot_list,plot2)
    #=================================================================================
    # RMS
    #=================================================================================
    # VALENCE
    RMS1 = 0
    N = length(data1$Time)
    for(i in c(1:N)){
      RMS1 = RMS1 + ((data1$vRoot[i] - data1$rRoot[i])^2)/N
    }
    
    # TOULOUSE
    RMS2 = 0
    N = length(data2$Time)
    for(i in c(1:N)){
      RMS2 = RMS2 + ((data2$vRoot[i] - data2$rRoot[i])^2)/N
    }
    
    # MOYENNE DES 3 + STOCKAGE
    RMS = mean(c(RMS1,RMS2))
    temp_df <- data.frame(kr = kr_in,
                          kx = kx_in,
                          krs = Krs,
                          RMS = RMS)
    RMS_df = rbind(RMS_df,temp_df)
    
  }
  write_csv(all_marshal, "outputs/all_marshal.csv")
}
write_csv(all_marshal, "outputs/all_marshal.csv")

#======================================
# GET THE BEST COMBINAISON
#======================================

min_RMS = min(RMS_df$RMS)
best_kx = (RMS_df %>% filter(RMS == min_RMS))$kx
best_kr = (RMS_df %>% filter(RMS == min_RMS))$kr
best_krs = (RMS_df %>% filter(RMS == min_RMS))$krs

print(paste0("La meilleure combinaison est : "))
print(paste0("Kx = ",best_kx))
print(paste0("Kr = ",best_kr))
print(paste0("Krs = ",best_krs," cm^3.hPa^-1.j^-1"))


#======================================
# PLOTS
#======================================
# Optionnal
is_true = T

if(is_true == T){
  indice = 1
  for(i in plot_list){
    print(i)
    #ggsave(paste0("plot_",indice,".png"))
    indice = indice + 1
  }
}

#======================================
# POTENTIEL AU COLLET
#======================================

# entrer le chemin du dossier hydrus Valence
H_list_v = get_h(paste0(Hydrus_Path,"Valence/Nod_Inf.out"))
# entrer le chemin du dossier hydrus Toulouse 
H_list_t = get_h(paste0(Hydrus_Path,"Toulouse/Nod_Inf.out"))

times = c(0.0010, 1.5,2.5,3.5,4.5,5.5,6.5,7)

# VALENCE
Pcollet_df_v = data.frame(Time = c(),PCollet_v = c())
for(i in c(1:length(times))){#boucle sur les 7 pas de temps
  
  #Calcul Heq
  Heq = 0
  H = H_list_v[i][[1]]
  for(j in c(1:length(H))){
    Heq = Heq + H[j]*SUF[j]
  }
  
  #Calcul Pcollet
  Tact = Tact = (data1 %>% filter(Time == times[i]))$vRoot
  temp = -(Tact/(Krs*75*15)) + Heq
  temp_df = data.frame(Time = times[i],
                       PCollet = temp)
  Pcollet_df_v = rbind(Pcollet_df_v, temp_df)
  
}

# TOULOUSE
Pcollet_df_t = data.frame(Time = c(),PCollet_t = c())
for(i in c(1:length(times))){#boucle sur les 7 pas de temps
  
  #Calcul Heq
  Heq = 0
  H = H_list_v[i][[1]]
  for(j in c(1:length(H))){
    Heq = Heq + H[j]*SUF[j]
  }
  
  #Calcul Pcollet (Couvreur 2012)
  Tact = Tact = (data2 %>% filter(Time == times[i]))$vRoot
  temp = -(Tact/(Krs*75*15)) + Heq
  temp_df = data.frame(Time = times[i],
                       PCollet = temp)
  Pcollet_df_t = rbind(Pcollet_df_t, temp_df)
  
}

Pcollet_plot <- ggplot(Pcollet_df_v, aes(Time, y = Pcollet)) + 
  geom_line(aes(y = PCollet), col = "red") +
  
  geom_line(data = Pcollet_df_t, aes(Time, y = PCollet), col = "blue") +
  ggtitle(paste0("Potentiel au collet [hPa]")) + 
  theme_set(theme_bw())

Pcollet_plot
ggsave("Potentiel au collet à midi.png")







