remove_outliers <- function(data, threshold = 2) {
  
  junk <- data.frame()
  
  for (temp_x in unique(data$x)){
    temp_data = data[(data$x == temp_x),]
    
    # Calculate Z-scores for each numeric column
    z_scores <- apply(temp_data[c(4,5,6,7)], 2, function(x) (x - mean(x)) / sd(x))
    
    # Identify rows with any Z-score above the threshold
    outlier_rows <- rowSums(abs(z_scores) > threshold) > 0
    
    # Find CS with outliers
    junk_temp <- unique(temp_data[outlier_rows, ][,c(1,2)])
    junk <- rbind(junk, junk_temp)
  }
  
  # Remove junk from Conductivities
  new_data <- anti_join(data, junk)
  
  return(new_data)
}

#===============================================================================
#===============================================================================

radius_pred <- function(newdata, radius_lm, 
                        lambda      = -0.62626, 
                        radius.mean = 0.1988, 
                        age.th      = 25){
  
  #' Return radius as linear function of age, timing and linear parameters
  
  # r_vec <- numeric(length(newdata$radius))
  # 
  # for(i in nrow(newdata)){
  #   
  #   temp_df <- newdata[i,]
  #   if(temp_df$age < age.th){
  #     r_vec[i] <- radius.mean
  #   }else{
  #     r_vec[i] <- BoxCox_reverse(predict(object = radius_lm, temp_df), 
  #                                lambda = lambda)
  #   }
  # }
  
  newdata2 <- newdata %>% 
    mutate(r_pred = radius.mean)
  
  newdata2$r_pred[newdata2$age >= age.th] <- BoxCox_reverse(predict(object = radius_lm, 
                                                                newdata%>%filter(age>=age.th)),
                                                        lambda = lambda
                                                        )
  
  newdata2$r_pred[newdata2$r_pred < radius.mean] <- radius.mean
  
  return(newdata2$r_pred)
}

#===============================================================================
#===============================================================================

kr_pred <- function(newdata, lm_kr, lambda = -0.42424){
  
  return(BoxCox_reverse(predict(lm_kr, newdata), lambda))
  
}

kx_pred <- function(newdata, lm_kx, lambda = -0.1414){
  
  return(BoxCox_reverse(predict(lm_kx, newdata), lambda))
  
}

#===============================================================================
#===============================================================================
# Define function to automatically generate plots
make_reg_data <- function(data, 
                          lm1, 
                          lm2 = NULL, 
                          transition.age = 25, 
                          split, 
                          error.ratio = 1,
                          age.min = 0,
                          age.max = 30
){
  base <- tibble(CS_id    = NaN,
                 param_id = unique(data$param_id),
                 value    = NaN,
                 age      = seq(age.min, age.max)
                 )
  df <- rbind(data, base) %>% 
    mutate(reg = 0,
           sd = 0)
  
  if(split){
    for(i in seq(1, length(df$age))){
      if(df$age[i] < transition.age){
        df$reg[i] <- predict(lm1, data.frame(age = df$age[i]))
        df$sd[i] <- sd(residuals((lm1)))
      }
      else{
        df$reg[i] <- exp(predict(lm2, data.frame(age = df$age[i])))
        # df$sd[i] <- error.ratio*exp(predict(lm2, data.frame(age = transition.age))) # /!\ 
        df$sd[i]  <- sd(residuals(lm2))
      }
    }  
  }
  else{
    for(i in seq(1, length(df$age))){
      df$reg[i] <- predict(lm1, data.frame(age = df$age[i]))
      df$sd[i] <- sd(residuals((lm1)))
    }    
  }
  return(df)
}
#===============================================================================
#===============================================================================
make_reg_plot <- function(df, 
                          name, 
                          path.out = "plots/regressions/"){
  
  xplot <- df %>% ggplot() +
    geom_ribbon(aes(x = age, ymin = reg-sd, ymax = reg+sd), fill = "orange", alpha = 0.5) +
    geom_line(aes(x = age, y = reg), color = "red", linetype = 2, linewidth = 1) +
    geom_point(aes(x = age, y = value)) +
    xlab("Age [d]") +
    ylab(paste0(name)) +
    theme_bw()
  
  plot(xplot)
  
  ggsave(filename = paste0(path.out, name, ".svg"), 
         plot = xplot, device = "svg", width = 4, height = 3)
  
  return(xplot)
  
}

#===============================================================================
#===============================================================================
# MERGE DISLOCATED CELLS IN CELLSET
merge_cells <- function(CellSet_sf, cell2merge){
  
  # Merge all cells indicated in cell2merge
  merged_sf_data <- list()
  for(CS_id_i in unique(CellSet_sf$CS_id)){
    
    section_data <- CellSet_sf[CellSet_sf$CS_id == CS_id_i, ]
    
    if(CS_id_i %in% names(cell2merge)){
      for(pair in cell2merge[[CS_id_i]]) {
        # Extract the polygons to merge
        polys_to_merge <- section_data[section_data$id_cell %in% pair, ]
        
        # Merge the polygons
        merged_poly <- st_union(polys_to_merge)
        
        # Replace the original polygons with the merged polygon
        new_row <- data.frame(
          CS_id    = CS_id_i,
          id_cell  = pair[1],
          geometry = list(merged_poly),
          stringsAsFactors = FALSE
        )
        
        # Replace the original polygons with the merged polygon
        section_data <- section_data %>%
          filter(!id_cell %in% pair) %>% 
          bind_rows(new_row)
      }
    }
    
    merged_sf_data[[CS_id_i]] <- section_data
  }
  
  # Combine the merged data
  CellSet_clean <- do.call(rbind, merged_sf_data)
  return(CellSet_clean)
}
#===============================================================================
#===============================================================================
sf_to_df <- function(CellSet_clean){
  CellSet_clean_df <- tibble(CS_id = character(),
                             id_cell = character(),
                             x = numeric(),
                             y = numeric()
  )
  
  for(i in seq(1, nrow(CellSet_clean))){
    
    cat("\r Iteration : ", sprintf("%04d", i), "/", nrow(CellSet_clean))
    flush.console()
    
    temp_coords <- as_tibble(st_coordinates(CellSet_clean$geometry[i]))
    
    temp_df <- tibble(CS_id = CellSet_clean$CS_id[i],
                      id_cell = CellSet_clean$id_cell[i],
                      x = temp_coords$X,
                      y = temp_coords$Y
    )
    
    CellSet_clean_df <- rbind(CellSet_clean_df, temp_df)
    
  }
  return(CellSet_clean_df)
}

# ============================================================================ #
# ============================================================================ #
BoxCox <- function(kr, lambda){
  
  if(lambda == 0){
    return(log(kr))
    
  }else if(lambda == 1){
    return(kr)
    
  }else{
    return((((kr^lambda) - 1)/lambda))
  }
}

BoxCox_reverse <- function(kr_tf, lambda){
  if(lambda == 0){
    return(exp(kr_tf))
  }else if(lambda == 1){
    return(kr_tf)
  }else{
    return((lambda*(kr_tf) + 1)^(1/lambda))
  }
}
# ============================================================================ #