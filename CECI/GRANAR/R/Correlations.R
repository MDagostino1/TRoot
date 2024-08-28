
plot_correlations_kr <- function(VR_conductivities, export_path = "Plots/", barrier = "b1"){

  Conds <- VR_conductivities %>% filter(Barrier == barrier)                        # select only b0 = no barrier apo
  Conds <- Conds[,-3]
  colnames(Conds)[c(1,2)] <- c("root", "x")
  
  HVR <- merge(x = Virtual_Roots, y = Conds, by = c("root", "x"))               # big table with VR and conductivities     
  HVR2 <- HVR %>% filter(root == HVR$root[1] & x == HVR$x[1])                   # subset that will be used in the loop
  HVR3 <- data.frame(Kr = Conds$Kr)                                             # create dataframe that will host data to calculate correlations
  Correlations <- data.frame()                                                  # create output dataframe
  
  # initiate loop
  for (i in seq(1:length(HVR2[,1]))) {
    temp_name <- HVR2$name[i]
    temp_type <- HVR2$type[i]
    
    # print(paste0("name : ", temp_name, " | type : ", temp_type))
    
    temp_HVR <- HVR %>% filter(name == temp_name, type == temp_type)            # contains all values for one particular parameter
    corr <- correlation(temp_HVR$y, temp_HVR$Kr)[2]                             # calculate correlations between the values of the param and kr
    
    temp_param <- paste0(temp_name, "_", temp_type)
    
    temp_correlation <- data.frame(param = temp_param,
                                   correlation = corr)
  
    Correlations <- rbind(Correlations, temp_correlation)                       # dataframe with only the correlation per param
    
    #=============================================================================
    
    y_df <- data.frame(temp_HVR$y)
    colnames(y_df) <- temp_param
    
    HVR3 <- cbind(HVR3, y_df)                                                   # final dataframe with all data in columns
      
  }
  
  Correlations2 <- t(Correlations[,-1])                                         # transpose Correlations
  # Correlations2 <- abs(Correlations2)
  colnames(Correlations2) <- Correlations$param
  Correlations2 <- cbind(data.frame(group = 1), Correlations2)
  
  p <- ggradar(Correlations2,
               # grid.min = min(Correlations$correlation), 
               grid.min = -1,
               # grid.max = max(Correlations$correlation),
               grid.max = 1,
               axis.label.size = 4,
               group.point.size = 5,
               grid.mid = 0
               # group.colours = c("#ece1e7","#f48fc8","#eb4da6","#d71984","#c4006e","#ffd0eb")
               )
  
  svg(filename = paste0(export_path, "Kr_correlations_radar.svg"), width = 10, height = 10)
  p
  dev.off()
  # ==============================================================================
  
  # corr_x <- correlation(Conds$x, Conds$Kr)[2]
  
  # HVR4 <- scale(cbind(x = Conds$x, HVR3))
  
  # corrplot(HVR4, method = "number")
  
  p2 <- ggplot(Correlations, aes(x = param, 
                                 y = abs(correlation), 
                                 color = abs(correlation), 
                                 size = 5, 
                                 shape = as.factor(round(correlation)))) +
    geom_point() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          legend.position="none") +
    scale_color_gradient(low="red", high="blue")
    
  
  # svg(filename = paste0(export_path, "Kr_correlations_points.svg"), width = 6, height = 4)
  # p2
  # dev.off()
  
  # print(paste0("Plots have been saved in ", export_path))
  
  return(list(p,p2))
  
}

plot_correlations_cdt <- function(VR_conductivities, export_path = "Plots/", barrier = "b1"){
  
  Conds <- VR_conductivities %>% filter(Barrier == barrier)                        # select only b0 = no barrier apo
  Conds <- Conds[,-3]  # drop barrier column
  # colnames(Conds)[c(1,2)] <- c("root", "x")
  
  HVR <- merge(x = Virtual_Roots, y = Conds, by = c("root", "x"))               # big table with VR and conductivities     
  HVR2 <- HVR %>% filter(root == HVR$root[1] & x == HVR$x[1])                   # subset that will be used in the loop
  HVR3 <- data.frame(Kr = Conds$Kr)                                             # create dataframe that will host data to calculate correlations
  Correlations <- data.frame()                                                  # create output dataframe
  
  # initiate loop
  for (i in seq(1:length(HVR2[,1]))) {
    temp_name <- HVR2$name[i]
    temp_type <- HVR2$type[i]
    
    # print(paste0("name : ", temp_name, " | type : ", temp_type))
    
    temp_HVR <- HVR %>% filter(name == temp_name, type == temp_type)            # contains all values for one particular parameter
    corr <- correlation(temp_HVR$y, temp_HVR$conductance)[2]                             # calculate correlations between the values of the param and kr
    
    temp_param <- paste0(temp_name, "_", temp_type)
    
    temp_correlation <- data.frame(param = temp_param,
                                   correlation = corr)
    
    Correlations <- rbind(Correlations, temp_correlation)                       # dataframe with only the correlation per param
    
    #=============================================================================
    
    y_df <- data.frame(temp_HVR$y)
    colnames(y_df) <- temp_param
    
    HVR3 <- cbind(HVR3, y_df)                                                   # final dataframe with all data in columns
    
  }
  
  Correlations2 <- t(Correlations[,-1])                                         # transpose Correlations
  # Correlations2 <- abs(Correlations2)
  colnames(Correlations2) <- Correlations$param
  Correlations2 <- cbind(data.frame(group = 1), Correlations2)
  
  p <- ggradar(Correlations2,
               # grid.min = min(Correlations$correlation), 
               grid.min = -1,
               # grid.max = max(Correlations$correlation),
               grid.max = 1,
               axis.label.size = 4,
               group.point.size = 5,
               grid.mid = 0
               # group.colours = c("#ece1e7","#f48fc8","#eb4da6","#d71984","#c4006e","#ffd0eb")
  )
  
  svg(filename = paste0(export_path, "Fluxes_correlations_radar.svg"), width = 10, height = 10)
  p
  dev.off()
  # ==============================================================================
  
  # corr_x <- correlation(Conds$x, Conds$Kr)[2]
  
  # HVR4 <- scale(cbind(x = Conds$x, HVR3))
  
  # corrplot(HVR4, method = "number")
  
  p2 <- ggplot(Correlations, aes(x = param, 
                                 y = abs(correlation), 
                                 color = abs(correlation), 
                                 size = 5, 
                                 shape = as.factor(round(correlation)))) +
    geom_point() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          legend.position="none") +
    scale_color_gradient(low="red", high="blue")
  
  
  # svg(filename = paste0(export_path, "Fluxes_correlations_points.svg"), width = 6, height = 4)
  # p2
  # dev.off()
  
  # print(paste0("Plots have been saved in ", export_path))
  
  return(list(p,p2))
  
}

plot_correlations_data <- function(anatomy2, export_path = "Plots/"){
  
  HVR2 <- anatomy2 %>% filter(CS_id == anatomy2$CS_id[1])                   # subset that will be used in the loop                                                # create output dataframe
  HVR3 <- data.frame(CS_id = unique(anatomy2$CS_id))
  
  # initiate loop
  for (i in seq(1:length(HVR2[,1]))) {
    temp_param <- HVR2$param_id[i]
    # print(paste0(i," temp_param : ", temp_param))
    temp_HVR <- anatomy2 %>% filter(param_id == temp_param)            # contains all values for one particular parameter
    y_df <- data.frame(temp_HVR$value)
    colnames(y_df) <- temp_param
    HVR3 <- cbind(HVR3, y_df)                                                   # final dataframe with all data in columns
  }
  
  # HVR4 <- HVR3[, c(7,8,9,11,12,13,14,15,17,19,22,25,28,31,32,33,34,35,36,37)]
  HVR4 <- HVR3[,c(4,5,7,8,11,14,15,18,21,22,23,27,28,29,30,32,33,34,35,36)]
  C <- cor(HVR4[,-1])
  p <- corrplot::corrplot(C)
  return(p)
}

plot_correlations_data3 <- function(anatomy3, export_path = "Plots/"){
  
  HVR2 <- anatomy3 %>% filter(CS_id == anatomy3$CS_id[1])                   # subset that will be used in the loop                                                # create output dataframe
  HVR3 <- data.frame(CS_id = unique(anatomy3$CS_id))
  
  # initiate loop
  for (i in seq(1:length(HVR2[,1]))) {
    temp_param <- HVR2$param_id[i]
    # print(paste0(i," temp_param : ", temp_param))
    temp_HVR <- anatomy3 %>% filter(param_id == temp_param)            # contains all values for one particular parameter
    y_df <- data.frame(temp_HVR$value)
    colnames(y_df) <- temp_param
    HVR3 <- cbind(HVR3, y_df)                                                   # final dataframe with all data in columns
  }
  
  # HVR4 <- HVR3[, c(7,8,9,11,12,13,14,15,17,19,22,25,28,31,32,33,34,35,36,37)]
  HVR4 <- HVR3[,c(4,5,7,8,11,14,15,18,21,22,23,27,28,29,30,32,33,34,35,36, 38)]
  C <- cor(HVR4[,-1])
  p <- corrplot::corrplot(C)
  return(p)
}

plot_correlations_simu <- function(Virtual_Roots, export_path = "Plots/"){
  
  VR2 <- merge(x = Virtual_Roots, y = Conductivities)
  HVR2 <- VR2 %>% filter(root == "root1" & x == 0 & Barrier == "b0")                   # subset that will be used in the loop                                                # create output dataframe
  HVR3 <- data.frame(conductance = VR2$conductance)
  
  # initiate loop
  for (i in seq(1:length(HVR2[,1]))) {
    temp_param <- HVR2$param_id[i]
    # print(paste0(i," temp_param : ", temp_param))
    temp_HVR <- anatomy2 %>% filter(param_id == temp_param)            # contains all values for one particular parameter
    y_df <- data.frame(temp_HVR$value)
    colnames(y_df) <- temp_param
    HVR3 <- cbind(HVR3, y_df)                                                      # final dataframe with all data in columns
  }
  
  # HVR4 <- HVR3[, c(7,8,9,11,12,13,14,15,17,19,22,25,28,31,32,33,34,35,36,37)]
  C <- cor(HVR3[,-1])
  p <- corrplot::corrplot(C)
  return(p)
}

plot_correlations_kr2 <- function(VR_conductivities, export_path = "Plots/", barrier = "b1"){
  
  Conds <- VR_conductivities %>% filter(Barrier == barrier)                        # select only b0 = no barrier apo
  Conds <- Conds[,-3]
  colnames(Conds)[c(1,2)] <- c("root", "x")
  
  HVR <- merge(x = Virtual_Roots, y = Conds, by = c("root", "x"))               # big table with VR and conductivities     
  HVR <- merge(x = HVR, y = Parameters[,c("name","type","param_id")], by = c("name","type"))
  HVR <- HVR[,c("root","x","y","Kr","Kx", "perimeter", "Kr2","param_id")]
  
  HVR2 <- HVR %>% filter(root == HVR$root[1] & x == HVR$x[1])                   # subset that will be used in the loop
  HVR3 <- data.frame(Kr = Conds$Kr)                                             # create dataframe that will host data to calculate correlations
  Correlations <- data.frame()                                                  # create output dataframe
  
  
  # initiate loop
  for (i in seq(1:length(HVR2[,1]))) {
    temp_param <- HVR2$param_id[i]
    
    # print(paste0("Param : ", temp_param))
    
    temp_HVR <- HVR %>% filter(param_id == temp_param)            # contains all values for one particular parameter
    corr <- correlation(temp_HVR$y, temp_HVR$Kr)[2]                             # calculate correlations between the values of the param and kr
    
    temp_correlation <- data.frame(param = temp_param,
                                   correlation = corr)
    
    Correlations <- rbind(Correlations, temp_correlation)                       # dataframe with only the correlation per param
    
    #=============================================================================
    
    y_df <- data.frame(temp_HVR$y)
    colnames(y_df) <- temp_param
    
    HVR3 <- cbind(HVR3, y_df)                                                   # final dataframe with all data in columns
    
  }
  
  Correlations2 <- t(Correlations[,-1])                                         # transpose Correlations
  # Correlations2 <- abs(Correlations2)
  colnames(Correlations2) <- Correlations$param
  Correlations2 <- cbind(data.frame(group = 1), Correlations2)
  
  p <- ggradar(Correlations2,
               # grid.min = min(Correlations$correlation), 
               grid.min = -1,
               # grid.max = max(Correlations$correlation),
               grid.max = 1,
               axis.label.size = 4,
               group.point.size = 5,
               grid.mid = 0
               # group.colours = c("#ece1e7","#f48fc8","#eb4da6","#d71984","#c4006e","#ffd0eb")
  )
  
  # svg(filename = paste0(export_path, "Kr_correlations_radar.svg"), width = 10, height = 10)
  p
  # dev.off()
  # ==============================================================================
  
  # corr_x <- correlation(Conds$x, Conds$Kr)[2]
  
  # HVR4 <- scale(cbind(x = Conds$x, HVR3))
  
  # corrplot(HVR4, method = "number")
  
  p2 <- ggplot(Correlations, aes(x = param, 
                                 y = correlation, 
                                 fill = correlation
                                 )) +
    # geom_point() +
    geom_bar(stat = "identity") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          legend.position="none") +
    scale_color_gradient(low="red", high="blue") +
    coord_flip()
  
  
  # svg(filename = paste0(export_path, "Kr_correlations_points.svg"), width = 6, height = 4)
  p2
  # dev.off()
  
  # print(paste0("Plots have been saved in ", export_path))
  
  return(list(p,p2))
  
}

