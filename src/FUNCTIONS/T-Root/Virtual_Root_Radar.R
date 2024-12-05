#' @
#' @param Virtual_Roots Table with the values of GRANAR param for each simulated virtual root
#' @param output_path The path where the plots will be stored. By default it is the current directory
#' @return Export radar plots

Virtual_Root_Radar <- function(Virtual_Roots, output_path = "", name = "radarplot"){

  # First we need to transform the data : each param must be the name of a column.
  # We need to have : Root | x | Xylem_n_files | ... | Epidermis_cell_diameter
  Virtual_Roots2 <- data.frame()
  # Initiate loop for x
  for (temp_x in unique(Virtual_Roots$x)) {
    # Initiate loop for root
    for(temp_root in unique(Virtual_Roots$root)){
      
      tempDF <- Virtual_Roots %>% filter(x == temp_x & root == temp_root)         # subset corresponding of one node of one root
      VR_temp <- data.frame(Root = tempDF$root[1], x = tempDF$x[1])               # df in which we will encode the data in the good form
       
      # Initiate loop per param.
      for(c in seq(1:length(tempDF[,c(1,2)]$name))){
        name_temp <- tempDF[c(1,2)][c,][,1]
        type_temp <- tempDF[c(1,2)][c,][,2]
        param_name <- paste0(name_temp, "_", type_temp)
        value <- tempDF$y[tempDF$name == name_temp & tempDF$type == type_temp]
        value_df <- data.frame(name = assign(paste0(param_name), value))
        colnames(value_df) <- paste0(param_name)
        VR_temp <- cbind(VR_temp, value_df)
      }
      
      Virtual_Roots2 <- rbind(Virtual_Roots2, VR_temp)
    }
  }
  
  #=============================================================================
  # plot %age, facet~root
  #=============================================================================
  VR3 <- Virtual_Roots2[,-1]
  # colnames(VR3)[1] <- "group"
  VR4 <- scale(VR3[,-1])
  VR4 <- data.frame(root = Virtual_Roots2[,1],age = VR3[,1], VR4)
  
  plotlist <- list()
  for (i in seq(1:length(unique(VR4$root)))) {
    VR5 <- VR4 %>% filter(root == unique(VR4$root)[i])
    colnames(VR5)[2] <- "group"
    p <- ggradar(VR5[,-1], 
                 grid.min = -3, 
                 grid.max = 3.5,
                 axis.label.size = 2,
                 group.point.size = 1,
                 group.colours = c("#ece1e7","#f48fc8","#eb4da6","#d71984","#c4006e","#ffd0eb"))
    assign(paste0("radarplot_",i), p) 
    
    plotlist <- append(plotlist,list(p))
    
  }
  
  svg(filename = paste0(output_path, name,"_1.svg"), width = 20, height = 20)
  gridExtra::grid.arrange(grobs = plotlist, ncol =2)
  dev.off()
  
  #=============================================================================
  # plot = %root, facet~age
  #=============================================================================
  
  plotlist <- list()
  for (i in seq(1:length(unique(VR4$age)))) {
    VR5 <- VR4 %>% filter(age == unique(VR4$age)[i])
    colnames(VR5)[1] <- "group"
    p <- ggradar(VR5[,-2], 
                 grid.min = -3, 
                 grid.max = 3.5,
                 axis.label.size = 2,
                 group.point.size = 1
                 # group.colours = c("#ece1e7","#f48fc8","#eb4da6","#d71984","#c4006e","#ffd0eb")
                 )
    assign(paste0("radarplot_",i), p) 
    
    plotlist <- append(plotlist,list(p))
    
  }
  
  svg(filename = paste0(output_path, name, "_2.svg"), width = 20, height = 20)
  gridExtra::grid.arrange(grobs = plotlist, ncol = 2)
  dev.off()
  
  print("The 2 plots have been created.")
  
}

# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================

# # First we need to transform the data : each param must be the name of a column.
# # We need to have : Root | x | Xylem_n_files | ... | Epidermis_cell_diameter
# Virtual_Roots2 <- data.frame()
# # Initiate loop for x
# for (temp_x in unique(Virtual_Roots$x)) {
#   # Initiate loop for root
#   for(temp_root in unique(Virtual_Roots$root)){
#     
#     tempDF <- Virtual_Roots %>% filter(x == temp_x & root == temp_root)         # subset corresponding of one node of one root
#     VR_temp <- data.frame(Root = tempDF$root[1], x = tempDF$x[1])               # df in which we will encode the data in the good form
#     
#     # Initiate loop per param.
#     for(c in seq(1:length(tempDF[,c(1,2)]$name))){
#       name_temp <- tempDF[c(1,2)][c,][,1]
#       type_temp <- tempDF[c(1,2)][c,][,2]
#       param_name <- paste0(name_temp, "_", type_temp)
#       value <- tempDF$y[tempDF$name == name_temp & tempDF$type == type_temp]
#       value_df <- data.frame(name = assign(paste0(param_name), value))
#       colnames(value_df) <- paste0(param_name)
#       VR_temp <- cbind(VR_temp, value_df)
#     }
#     
#     Virtual_Roots2 <- rbind(Virtual_Roots2, VR_temp)
#   }
# }

#=============================================================================
# plot %age, facet~root
#=============================================================================

