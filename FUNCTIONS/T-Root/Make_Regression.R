# This function creates automatically the table "Regressions"
# NB : To perform and visualize statistics, check the Regressions.Rmd file.

#' Create a table with the regressions of each param
#'
#' @param anatomy2 Table with all quantification per cross sections in the form of GRANAR params
#' @export
#' @examples
#'

Make_Regression <- function(anatomy2){
  
  Regressions <- data.frame()
  
  anatomy2E <- merge(anatomy2, CrossSections[c("CS_id", "age", "plant_id")])
  anatomy2E <- merge(anatomy2E, Parameters[c("param_id", "name","type")])
  
  #=============================================================================
  # XYLEM
  #=============================================================================
  
  # Data preparation
  xylem_stats <- anatomy2E %>% filter(name == "xylem")
  xylem_stats <- merge(x = xylem_stats, y = CrossSections[c("CS_id", "age", "plant_id")])
  
  xylem_n_cells <- xylem_stats %>% filter(type == "n_cells") 
  xylem_max_size <- xylem_stats %>% filter(type == "max_size")
  xylem_mean_size <- xylem_stats %>% filter(type == "mean_size")
  
  
  
  #=============================================================================
  # 1) n_cells
  
  # Apply ln
  xylem_n_cells$value_ln <- ln(xylem_n_cells$value)

  # Linear model
  XNF_lm1 <- lm(data = xylem_n_cells, formula = value_ln~age)
  # summary(XNF_lm1)
  # visreg(XNF_lm1)
  # plot(XNF_lm1)

  # Coefficients
  a = round(XNF_lm1$coefficients[[2]], 5)
  b = round(XNF_lm1$coefficients[[1]], 5)
  sigma = round(summary(XNF_lm1)$sigma, 5)
  
  # Create temp dataframe
  temp <- data.frame(name = "xylem",
                     type = "n_cells",
                     reg_param = c("Fx", "sigma"),
                     value = c(paste0(""),
                               sd(xylem_n_cells$value))
  )
  
  # Add data
  Regressions <- rbind(Regressions, temp)
  
  #=============================================================================
  # 2) Mean_Size
  
  # Apply ln
  xylem_mean_size$value_ln <- ln(xylem_mean_size$value)
  
  XMeS_lm1 <- lm(data = xylem_mean_size, formula = value_ln~age)
  # plot(XMeS_lm1)
  # summary(XMeS_lm1)
  # visreg(XMeS_lm1)
  
  # Coefficients
  XMeS_a = round(XMeS_lm1$coefficients[[2]], 5)
  XMeS_b = round(XMeS_lm1$coefficients[[1]], 5)
  XMeS_sigma = round(summary(XMeS_lm1)$sigma, 5)
  
  # Save
  temp <- data.frame(name = "xylem",
                     type = "mean_size",
                     reg_param = c("Fx", "sigma"),
                     value = c(paste0("exp(",XMeS_a,"*X + ",XMeS_b," + sd)"),
                               XMeS_sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  #=============================================================================
  
  # 3) Max_Size
  
  # There is a strong correlation between max_size and mean_size
  # We will keep mean_size because it has a higher correlation with kr, and 
  # set max_size as a function of mean_size, translated in a function of x
  
  # For that, we construct a linear regression of max_size around mean_size
  xdf <- data.frame(mean_size = xylem_mean_size$value, 
                    max_size = xylem_max_size$value)
  xcor <- cor(xdf)
  print(paste0("Correlation between xylem_mean_size and xylem_max_size is : ", round(xcor[2], 3)))
  
  
  # Linear model
  XMS_lm <- lm(data = xdf, formula = max_size ~ 0 + mean_size)
  # plot(xdf$max_size, xdf$mean_size)
  # plot(XMS_lm)
  # summary(XMS_lm)
  # visreg(XMS_lm)
  
  # Coefficients
  a <- round(XMS_lm$coefficients[[1]], 5)
  sigma <- round(summary(XMS_lm)$sigma, 5)
  
  # We express xylem_max_size in a functino of x :
  a2 <- paste0(a,
               "*",
               "exp(",XMeS_a,"*X + ",XMeS_b,")",
               "+ sd")
  
  # Save
  temp <- data.frame(name = "xylem",
                     type = "max_size",
                     reg_param = c("Fx", "sigma"),
                     value = c(a2, sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  #=============================================================================
  # STELE
  #=============================================================================
  
  # Data preparation
  
  stele_stats <- anatomy2E %>% filter(name == "stele")
  
  stele_stats <- merge(x = stele_stats, y = CrossSections[c("CS_id", "age", "plant_id")])
  stele_cell_diameter <- stele_stats %>% filter(type == "cell_diameter") 
  stele_layer_diameter <- stele_stats %>% filter(type == "layer_diameter")
  stele_mean_size <- stele_stats %>% filter(type == "mean_size")
  stele_n_layers <- stele_stats %>% filter(type == "n_layers")
  stele_SD <- stele_stats %>% filter(type == "SD")
  stele_totarea <- stele_stats %>% filter(type == "totarea")
  
  # For the stele, we have six parameters.
  # However, we can deduce four of them from layer_diameter because they are very highly correlated (>0.9)
  # We will then only use layer_diameter and n_layers as basic parameters that will go into the analysis
  
  #=============================================================================
  # 1) Layer_diameter
  
  # Apply ln
  stele_layer_diameter$value_ln <- ln(stele_layer_diameter$value)
  
  # Linear model
  SLD_lm1 <- lm(data = stele_layer_diameter, formula = value_ln~age)
  # plot(SLD_lm1)
  # summary(SLD_lm1)
  # visreg(SLD_lm1)
  
  # Coefficients
  SLD_a = round(SLD_lm1$coefficients[[2]], 5)
  SLD_b = round(SLD_lm1$coefficients[[1]], 5)
  sigma = round(summary(SLD_lm1)$sigma, 5)
  
  # Save
  temp <- data.frame(name = "stele",
                     type = "layer_diameter",
                     reg_param = c("Fx", "sigma"),
                     value = c(paste0("exp(",SLD_a,"*X + ",SLD_b," + sd)"),
                               sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  
  #=============================================================================
  # We create a df with the 5 correlated stele parameters
  sdf <- data.frame(layer_diameter = stele_layer_diameter$value, 
                    cell_diameter = stele_cell_diameter$value,
                    mean_size = stele_mean_size$value,
                    SD = stele_SD$value,
                    totarea = stele_totarea$value)
  scor <- cor(sdf)
  print("Correlation for stele parameters are : ")
  print(scor)
  #=============================================================================
  # CELL_DIAMETER
  
  # Linear model
  scd_lm <- lm(data = sdf, formula =  cell_diameter ~ 0 + layer_diameter)
  # plot(sdf$cell_diameter, sdf$layer_diameter)
  # plot(scd_lm)
  # summary(XMS_lm)
  # visreg(XMS_lm)
  
  # Coefficients
  a <- round(scd_lm$coefficients[[1]], 5)
  sigma <- round(summary(scd_lm)$sigma, 5)
  
  # We express stele_cell_diameter in a functinon of x :
  a2 <- paste0(a,
               "*",
               "exp(",SLD_a,"*X + ",SLD_b,")",
               "+ sd")
  
  # Save
  temp <- data.frame(name = "stele",
                     type = "cell_diameter",
                     reg_param = c("Fx", "sigma"),
                     value = c(a2, sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  #=============================================================================
  # MEAN_SIZE
  
  # Linear model
  sms_lm <- lm(data = sdf, formula =  mean_size ~ 0 + layer_diameter)
  # plot(sdf$mean_size, sdf$layer_diameter)
  # plot(sms_lm)
  # summary(sms_lm)
  # visreg(sms_lm)
  
  # Coefficients
  a <- round(sms_lm$coefficients[[1]], 5)
  sigma <- round(summary(sms_lm)$sigma, 5)
  
  # We express stele_mean_size in a functinon of x :
  a2 <- paste0(a,
               "*",
               "exp(",SLD_a,"*X + ",SLD_b,")",
               "+ sd")
  
  # Save
  temp <- data.frame(name = "stele",
                     type = "mean_size",
                     reg_param = c("Fx", "sigma"),
                     value = c(a2, sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  # ===========================================================================
  # SD
  
  # Linear model
  sd_lm <- lm(data = sdf, formula =  SD ~ 0 + layer_diameter)
  # plot(sdf$mean_size, sdf$layer_diameter)
  # plot(sd_lm)
  # summary(sd_lm)
  # visreg(sd_lm)
  
  # Coefficients
  a <- round(sd_lm$coefficients[[1]], 5)
  sigma <- round(summary(sd_lm)$sigma, 5)
  
  # We express stele_mean_size in a functinon of x :
  a2 <- paste0(a,
               "*",
               "exp(",SLD_a,"*X + ",SLD_b,")",
               "+ sd")
  
  # Save
  temp <- data.frame(name = "stele",
                     type = "SD",
                     reg_param = c("Fx", "sigma"),
                     value = c(a2, sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  #=============================================================================
  # TOTAREA
  
  # Obviously, totarea seems exponentially correlated with layer_diameter
  
  # Linear model
  ta_lm <- lm(data = sdf, formula =  totarea ~ 0 + I(layer_diameter^2))
  # plot((sdf$layer_diameter)^2, sdf$totarea)
  # plot(sd_lm)
  # summary(ta_lm)
  # visreg(ta_lm)
  
  # After some statistics, we see that totarea is extremely correlated with layer_diameter^2
  # So we set an equation for totarea :
  
  # Coefficients
  a <- round(ta_lm$coefficients[[1]], 5)
  sigma <- round(summary(ta_lm)$sigma, 5)
  
  # We express stele_mean_size in a function of x :
  a2 <- paste0(a,
               "*",
               "((exp(",SLD_a,"*X + ",SLD_b,"))^2)",
               "+ sd")
  
  # Save
  temp <- data.frame(name = "stele",
                     type = "totarea",
                     reg_param = c("Fx", "sigma"),
                     value = c(a2, sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  
  #=============================================================================
  # 4) N_layers
  
  # Apply ln
  stele_n_layers$value_ln <- ln(stele_n_layers$value)
  
  # Linear model
  SNL_lm1 <- lm(data = stele_n_layers, formula = value_ln~age)
  # plot(SNL_lm1)
  # summary(SNL_lm1)
  # visreg(SNL_lm1)
  
  # Coefficients
  a = round(SNL_lm1$coefficients[[2]], 5)
  b = round(SNL_lm1$coefficients[[1]], 5)
  sigma = round(summary(SNL_lm1)$sigma, 5)
  
  # Save
  temp <- data.frame(name = "stele",
                     type = "n_layers",
                     reg_param = c("Fx", "sigma"),
                     value = c(paste0("exp(",a,"*X + ",b," + sd)"),
                               sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  
  
  #=============================================================================
  # PHLOEM
  #=============================================================================
  
  # Data preparation
  
  phloem_stats <- anatomy2E %>% filter(name == "phloem")
  phloem_stats <- merge(x = phloem_stats, y = CrossSections[c("CS_id", "age", "plant_id")])
  phloem_n_files <- phloem_stats %>% filter(type == "n_files") 
  phloem_max_size <- phloem_stats %>% filter(type == "max_size")
  phloem_proportion <- phloem_stats %>% filter(type == "proportion")
  
  #=============================================================================
  # 1) N_Files
  
  # Apply ln
  phloem_n_files$value_ln <- ln(phloem_n_files$value)
  
  # Linear model
  lm1 <- lm(data = phloem_n_files, formula = value_ln~age)
  # plot(lm1)
  # summary(lm1)
  # visreg(lm1)
  
  # Coefficients
  a = round(lm1$coefficients[[2]], 5)
  b = round(lm1$coefficients[[1]], 5)
  sigma = round(summary(lm1)$sigma, 5)
  
  # Save
  temp <- data.frame(name = "phloem",
                     type = "n_files",
                     reg_param = c("Fx", "sigma"),
                     value = c(paste0("exp(",a,"*X + ",b,"+ sd)"),
                               sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  #=============================================================================
  # 2) Max_Size
  
  # Linear model
  lm1 <- lm(data = phloem_max_size, formula = value~age)
  # plot(lm1)
  # summary(lm1)
  # visreg(lm1)
  
  # Coefficients
  a = round(lm1$coefficients[[2]], 5)
  b = round(lm1$coefficients[[1]], 5)
  sigma = round(summary(lm1)$sigma, 5)
  
  # Save
  temp <- data.frame(name = "phloem",
                     type = "max_size",
                     reg_param = c("Fx", "sigma"),
                     value = c(paste0(a,"*X + ",b, " + sd"),
                               sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  #=============================================================================
  # 3) Proportion
  
  # Linear model
  lm1 <- lm(data = phloem_proportion, formula = value~age)
  # plot(lm1)
  # summary(lm1)
  # visreg(lm1)
  
  # Coefficients
  a = round(lm1$coefficients[[2]], 5)
  b = round(lm1$coefficients[[1]], 5)
  sigma = round(summary(lm1)$sigma, 5)
  
  # Save
  temp <- data.frame(name = "phloem",
                     type = "proportion",
                     reg_param = c("Fx", "sigma"),
                     value = c(paste0(a,"*X + ",b, " + sd"),
                               sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  #=============================================================================
  # PERICYCLE
  #=============================================================================
  
  # Data preparation
  pericycle_stats <- anatomy2E %>% filter(name == "pericycle")
  pericycle_stats <- merge(x = pericycle_stats, y = CrossSections[c("CS_id", "age", "plant_id")])
  pericycle_cell_diameter <- pericycle_stats %>% filter(type == "cell_diameter") 
  
  #=============================================================================
  # 1) Cell_dimater
  
  # Linear model
  lm1 <- lm(data = pericycle_cell_diameter, formula = value~age)
  # plot(lm1)
  # summary(lm1)   # Catastrophic prediction :/
  # visreg(lm1)
  
  # Coefficients
  a = round(lm1$coefficients[[2]], 5)
  b = round(lm1$coefficients[[1]], 5)
  sigma = round(summary(lm1)$sigma, 5)
  
  # Save
  temp <- data.frame(name = "pericycle",
                     type = "cell_diameter",
                     reg_param = c("Fx", "sigma"),
                     value = c(paste0(a,"*X + ",b, " + sd"),
                               sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  #=============================================================================
  # ENDODERMIS
  #=============================================================================
  
  # Data preparation
  endodermis_stats <- anatomy2E %>% filter(name == "endodermis")
  endodermis_stats <- merge(x = endodermis_stats, y = CrossSections[c("CS_id", "age", "plant_id")])
  endodermis_barrier <- endodermis_stats %>% filter(type == "barrier")
  endodermis_cell_diameter <- endodermis_stats %>% filter(type == "cell_diameter")
  
  #=============================================================================
  # 1) Cell_diameter
  
  # Apply ln
  endodermis_cell_diameter$value_ln <- ln(endodermis_cell_diameter$value) # no need here
  
  # Linear model
  lm1 <- lm(data = endodermis_cell_diameter, formula = value_ln~age)
  # plot(lm1)
  # summary(lm1)   # Catastrophic prediction :/
  # visreg(lm1)
  
  # Coefficients
  a = round(lm1$coefficients[[2]], 5)
  b = round(lm1$coefficients[[1]], 5)
  sigma = round(summary(lm1)$sigma, 5)
  
  # Save
  temp <- data.frame(name = "endodermis",
                     type = "cell_diameter",
                     reg_param = c("Fx", "sigma"),
                     value = c(paste0("exp(",a,"*X + ",b," + sd)"),
                               sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  #=============================================================================
  # CORTEX
  #=============================================================================
  
  cortex_stats <- anatomy2E %>% filter(name == "cortex")
  cortex_stats <- merge(x = cortex_stats, y = CrossSections[c("CS_id", "age", "plant_id")])
  cortex_n_layers <- cortex_stats %>% filter(type == "n_layers") 
  cortex_cell_diameter <- cortex_stats %>% filter(type == "cell_diameter")
  
  #=============================================================================
  # 1) N_layers
  
  # Linear model
  lm1 <- lm(data = cortex_n_layers, formula = value~age)
  # plot(lm1)
  # summary(lm1)  
  # visreg(lm1)
  
  # Coefficients
  a = round(lm1$coefficients[[2]], 5)
  b = round(lm1$coefficients[[1]], 5)
  sigma = round(summary(lm1)$sigma, 5)
  
  # Save
  temp <- data.frame(name = "cortex",
                     type = "n_layers",
                     reg_param = c("Fx", "sigma"),
                     value = c(paste0(a,"*X + ",b, " + sd"),
                               sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  #=============================================================================
  # 2) Cell_diameter
  
  # Apply ln
  cortex_cell_diameter$value_ln <- ln(cortex_cell_diameter$value) 
  
  # Linear model
  lm1 <- lm(data = cortex_cell_diameter, formula = value_ln~age)
  # plot(lm1)
  # summary(lm1)   # Catastrophic prediction :/
  # visreg(lm1)
  
  # Coefficients
  a = round(lm1$coefficients[[2]], 5)
  b = round(lm1$coefficients[[1]], 5)
  sigma = round(summary(lm1)$sigma, 5)
  
  # Save
  temp <- data.frame(name = "cortex",
                     type = "cell_diameter",
                     reg_param = c("Fx", "sigma"),
                     value = c(paste0("exp(",a,"*X + ",b," + sd)"),
                               sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  #=============================================================================
  # EXODERMIS
  #=============================================================================
  
  # Data preparation
  exodermis_stats <- anatomy2E %>% filter(name == "exodermis")
  exodermis_stats <- merge(x = exodermis_stats, y = CrossSections[c("CS_id", "age", "plant_id")])
  exodermis_barrier <- exodermis_stats %>% filter(type == "barrier")
  exodermis_cell_diameter <- exodermis_stats %>% filter(type == "cell_diameter")
  
  #=============================================================================
  # 1) Cell_diameter
  
  # Apply ln
  exodermis_cell_diameter$value_ln <- ln(exodermis_cell_diameter$value) 
  
  # Linear model
  lm1 <- lm(data = exodermis_cell_diameter, formula = value_ln~age)
  # plot(lm1)
  # summary(lm1)   # Catastrophic prediction :/
  # visreg(lm1)
  
  # Coefficients
  a = round(lm1$coefficients[[2]], 5)
  b = round(lm1$coefficients[[1]], 5)
  sigma = round(summary(lm1)$sigma, 5)
  
  # Save
  temp <- data.frame(name = "exodermis",
                     type = "cell_diameter",
                     reg_param = c("Fx", "sigma"),
                     value = c(paste0("exp(",a,"*X + ",b," + sd)"),
                               sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  #=============================================================================
  # EPIDERMIS
  #=============================================================================
  
  # Data preparation
  epidermis_stats <- anatomy2E %>% filter(name == "epidermis")
  epidermis_stats <- merge(x = epidermis_stats, y = CrossSections[c("CS_id", "age", "plant_id")])
  epidermis_cell_diameter <- epidermis_stats %>% filter(type == "cell_diameter")
  
  #=============================================================================
  # 1) Cell_dimater
  
  # Apply ln
  epidermis_cell_diameter$value_ln <- ln(epidermis_cell_diameter$value) 
  
  # Linear model
  lm1 <- lm(data = epidermis_cell_diameter, formula = value_ln~age)
  # plot(lm1)
  # summary(lm1)   # Catastrophic prediction :/
  # visreg(lm1)
  
  # Coefficients
  a = round(lm1$coefficients[[2]], 5)
  b = round(lm1$coefficients[[1]], 5)
  sigma = round(summary(lm1)$sigma, 5)
  
  # Save
  temp <- data.frame(name = "epidermis",
                     type = "cell_diameter",
                     reg_param = c("Fx", "sigma"),
                     value = c(paste0("exp(",a,"*X + ",b," + sd)"),
                               sigma)
  )
  
  Regressions <- rbind(Regressions, temp)
  
  Regressions <- merge(Regressions, Parameters[c("name", "type", "param_id")])
  Regressions <- Regressions[c("param_id", "reg_param", "value")]
  
  #=============================================================================
  #=============================================================================
  #=============================================================================
  
  return(Regressions)
  
}


  
  