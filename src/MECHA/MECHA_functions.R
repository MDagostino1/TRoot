# ============================================================================ #
# TRANSFORM DATAFRAME INTO LIST
# ============================================================================ #

list_MECHA_inputs <- function(MECHA_params){
  
  final_list <- list()
  
  # LOOP PER FAMILY (General, Geometry, BC, Hormones, Hydraulics)
  for(f in unique(MECHA_params$family)){
    
    # LOOP PER PARAM TYPE
    for(t in unique(MECHA_params$type[MECHA_params$family == f])){
      
      # cat("f : ", f, " | t : ", t, "\n")
      
      subset <- MECHA_params %>% filter(family == f & type == t)
      
      for(row_i in seq(1:nrow(subset))){
        
        subrow <- subset[row_i,]
        
        # If there is no subtype -> directly write to name and key
        if(is.na(subrow$subtype1)){
          
          if(subrow$format == "character"){
            final_list[[subrow$family]][[subrow$type]][[subrow$name]][[subrow$key]] = as.character(subrow$default_value)
          }
          
          if(subrow$format == "integer"){
            final_list[[subrow$family]][[subrow$type]][[subrow$name]][[subrow$key]] = as.integer(subrow$default_value)
          }
          
          if(subrow$format == "float"){
            final_list[[subrow$family]][[subrow$type]][[subrow$name]][[subrow$key]] = as.double(subrow$default_value)
          }
          
          if(!(subrow$format %in% c("character", "float", "integer"))){
            stop("Format unknown.")
          }
          
        }else{
          
          if(is.na(subrow$subtype2)){
            
            if(subrow$format == "character"){
              final_list[[subrow$family]][[subrow$type]][[subrow$subtype1]][[subrow$name]][[subrow$key]] = as.character(subrow$default_value)
            }
            
            if(subrow$format == "integer"){
              final_list[[subrow$family]][[subrow$type]][[subrow$subtype1]][[subrow$name]][[subrow$key]] = as.integer(subrow$default_value)
            }
            
            if(subrow$format == "float"){
              final_list[[subrow$family]][[subrow$type]][[subrow$subtype1]][[subrow$name]][[subrow$key]] = as.double(subrow$default_value)
            }
            
            if(!(subrow$format %in% c("character", "float", "integer"))){
              stop("Format unknown.")
            }
            
          }else{
            
            if(is.na(subrow$subtype3)){
              
              if(subrow$format == "character"){
                final_list[[subrow$family]][[subrow$type]][[subrow$subtype1]][[subrow$subtype2]][[subrow$name]][[subrow$key]] = as.character(subrow$default_value)
              }
              
              if(subrow$format == "integer"){
                final_list[[subrow$family]][[subrow$type]][[subrow$subtype1]][[subrow$subtype2]][[subrow$name]][[subrow$key]] = as.integer(subrow$default_value)
              }
              
              if(subrow$format == "float"){
                final_list[[subrow$family]][[subrow$type]][[subrow$subtype1]][[subrow$subtype2]][[subrow$name]][[subrow$key]] = as.double(subrow$default_value)
              }
              
              if(!(subrow$format %in% c("character", "float", "integer"))){
                stop("Format unknown.")
              }
              
            }else{
              
              final_list[[subrow$family]][[subrow$type]][[subrow$subtype1]][[subrow$subtype2]][[subrow$subtype3]][[subrow$name]][[subrow$key]] = subrow$default_value
              if(subrow$format == "character"){
                final_list[[subrow$family]][[subrow$type]][[subrow$subtype1]][[subrow$subtype2]][[subrow$subtype3]][[subrow$name]][[subrow$key]] = as.character(subrow$default_value)
              }
              
              if(subrow$format == "integer"){
                final_list[[subrow$family]][[subrow$type]][[subrow$subtype1]][[subrow$subtype2]][[subrow$subtype3]][[subrow$name]][[subrow$key]] = as.integer(subrow$default_value)
              }
              
              if(subrow$format == "float"){
                final_list[[subrow$family]][[subrow$type]][[subrow$subtype1]][[subrow$subtype2]][[subrow$subtype3]][[subrow$name]][[subrow$key]] = as.double(subrow$default_value)
              }
              
              if(!(subrow$format %in% c("character", "float", "integer"))){
                stop("Format unknown.")
              }
              
              
            }}} 
      }
    }
  }
  
  return(final_list)
}

# ============================================================================ #
# TRANSFORM LIST INTO XML
# ============================================================================ #

# list_to_xml <- function(lst, node_name = "root") {
#   node <- xml_new_root(node_name)
#   
#   add_children <- function(parent, sublist) {
#     for (name in names(sublist)) {
#       value <- sublist[[name]]
#       
#       if (is.list(value)) {
#         # If all elements are simple (not lists), store as attributes
#         if (all(!sapply(value, is.list))) {
#           child <- xml_add_child(parent, name)
#           for (attr_name in names(value)) {
#             attr_value <- value[[attr_name]]
#             if (is.integer(attr_value)) {
#               xml_set_attr(child, attr_name, sprintf("%d", attr_value))  # Integer format
#             } else if (is.double(attr_value)) {
#               xml_set_attr(child, attr_name, sprintf("%.1f", attr_value))  # Float format
#             } else {
#               xml_set_attr(child, attr_name, as.character(attr_value))  # Default
#             }
#           }
#         } else {
#           child <- xml_add_child(parent, name)
#           add_children(child, value)
#         }
#       } else {
#         # Encode integer and float attributes correctly
#         if (is.integer(value)) {
#           xml_set_attr(parent, name, sprintf("%d", value))  # Integer
#         } else if (is.double(value)) {
#           xml_set_attr(parent, name, sprintf("%.1f", value))  # Float
#         } else {
#           xml_set_attr(parent, name, as.character(value))  # Default (string)
#         }
#       }
#     }
#   }
#   
#   add_children(node, lst)
#   return(node)
# }

list_to_xml <- function(lst, node_name = "root") {
  node <- xml_new_root(node_name)
  
  add_children <- function(parent, sublist) {
    for (name in names(sublist)) {
      value <- sublist[[name]]
      
      if (is.list(value)) {
        # Case: List of unnamed elements (e.g., multiple <K_sieve> elements)
        if (is.null(names(value))) {
          for (item in value) {
            child <- xml_add_child(parent, name)
            for (attr_name in names(item)) {
              attr_value <- item[[attr_name]]
              if (is.integer(attr_value)) {
                xml_set_attr(child, attr_name, sprintf("%d", attr_value))  # Integer
              } else if (is.double(attr_value)) {
                xml_set_attr(child, attr_name, sprintf("%E", attr_value))  # Float (scientific notation)
              } else {
                xml_set_attr(child, attr_name, as.character(attr_value))  # String
              }
            }
          }
        } else {
          # Case: Named sublists (nested structure)
          child <- xml_add_child(parent, name)
          add_children(child, value)
        }
      } else {
        # Case: Single attributes
        xml_set_attr(parent, name, as.character(value))
      }
    }
  }
  
  add_children(node, lst)
  return(node)
}

# ============================================================================ #
# REPLACE A SPECIFIC ITEM IN A LIST KNOWING ITS PARAM_NAME AND PARAM_KEY
# ============================================================================ #

my_replace_fn <- function(lst_sub, param_name, param_key, new_value) {
  
  lapply(lst_sub, function(x) {
    if (is.list(x)) {
      my_replace_fn(x, param_name, param_key, new_value)  # Recurse into sublist
    } else {
      x  # Keep other elements unchanged
    }
  }) -> lst_sub
  
  if (!is.null(lst_sub[[param_name]][[param_key]])) {
    lst_sub[[param_name]][[param_key]] <- new_value  # Replace if key exists at this level
  }
  
  return(lst_sub)
}

MECHA_input_replace <- function(lst, family, param_name, param_key, new_value){
  lst_sub <- lst[[family]]
  lst_sub <- my_replace_fn(lst_sub, param_name, param_key, new_value)
  lst[[family]] <- lst_sub
  return(lst)
}

# ============================================================================ #
# CONVER XML TO NEST LIST
# ============================================================================ #

xml_to_list <- function(node) {
  result <- list()
  
  # Read attributes and convert types (integer, double, or string)
  attrs <- xml_attrs(node)
  if (length(attrs) > 0) {
    for (name in names(attrs)) {
      value <- attrs[[name]]
      
      # Convert value to the correct type (integer, double, or string)
      if (grepl("^\\d+$", value)) {
        result[[name]] <- as.integer(value)  # Integer
      } else if (grepl("^\\d+\\.\\d+E?-?\\d*$", value)) {
        result[[name]] <- as.double(value)  # Float (handles scientific notation)
      } else {
        result[[name]] <- value  # String
      }
    }
  }
  
  # Process child nodes
  children <- xml_children(node)
  if (length(children) > 0) {
    for (child in children) {
      child_name <- xml_name(child)
      child_list <- xml_to_list(child)  # Recursive call
      
      # If there are multiple children with the same name, store them in a list
      if (child_name %in% names(result)) {
        if (!is.list(result[[child_name]]) || !is.null(names(result[[child_name]]))) {
          result[[child_name]] <- list(result[[child_name]])  # Convert to list if needed
        }
        result[[child_name]] <- append(result[[child_name]], list(child_list))
      } else {
        result[[child_name]] <- child_list
      }
    }
  }
  
  return(result)
}

# ============================================================================ #
# READ ALL INPUT FILES FROM A PATH
# ============================================================================ #

read_MECHA_inputs <- function(read_path, 
                              file_General    = "Arabido1_General_traces.xml",
                              file_Hydraulics = "Hydraulics_optim.xml",
                              file_Hormones   = "Hormones_optim.xml",
                              file_BC         = "BC_optim.xml",
                              file_Geometry   = "Geometry_optim.xml"){
  
  # Load all xml input files from read_path
  General    <- xml_to_list(read_xml(paste0(read_path, file_General)))
  Hydraulics <- xml_to_list(read_xml(paste0(read_path, file_Hydraulics)))
  Hormones   <- xml_to_list(read_xml(paste0(read_path, file_Hormones)))
  BC         <- xml_to_list(read_xml(paste0(read_path, file_BC)))
  Geometry   <- xml_to_list(read_xml(paste0(read_path, file_Geometry)))
  
  # Make them into a big list
  MECHA_inputs_list <- list(
    General    = list(properties = General),
    Geometry   = list(param      = Geometry),
    Hydraulics = list(param      = Hydraulics),
    BC         = list(properties = BC),
    Hormones   = list(param      = Hormones)
  )
  
  return(MECHA_inputs_list)
}

# ============================================================================ #
# WRITE MECHA INPUTS
# ============================================================================ #

write_MECHA_inputs <- function(input_list, 
                               path, 
                               file_General    = "Arabido1_General_traces.xml",
                               file_Hydraulics = "Hydraulics_optim.xml",
                               file_Hormones   = "Hormones_optim.xml",
                               file_BC         = "BC_optim.xml",
                               file_Geometry   = "Geometry_optim.xml"){
  
  General    <- list_to_xml(input_list[["General"]][["properties"]], node_name = "properties")
  Geometry   <- list_to_xml(input_list[["Geometry"]][["param"]],     node_name = "param")
  Hydraulics <- list_to_xml(input_list[["Hydraulics"]][["param"]],   node_name = "param")
  Hormones   <- list_to_xml(input_list[["Hormones"]][["param"]],     node_name = "param")
  BC         <- list_to_xml(input_list[["BC"]][["properties"]],      node_name = "properties")
  
  write_xml(General,    file = paste0(path, file_General))
  write_xml(Geometry,   file = paste0(path, file_Geometry))
  write_xml(Hydraulics, file = paste0(path, file_Hydraulics))
  write_xml(Hormones,   file = paste0(path, file_Hormones))
  write_xml(BC,         file = paste0(path, file_BC))
  
}

# ============================================================================ #
# ============================================================================ #
update_MECHA_inputs <- function(MECHA_inputs_list){
  # SET PATHS
  MECHA_inputs_list$Geometry$param$path$value                                     <- "Daniela/Arabido4bis.xml" # XML Cross section file
  # MECHA_inputs_list$General$properties$Output$path                                <- output_path               # Output folder 
  MECHA_inputs_list$Geometry$param$Plant$value                                    <- ""
  MECHA_inputs_list$BC$properties$path_scenarios$Output$path                      <- ""
  MECHA_inputs_list$Hydraulics$param$path_hydraulics$Output$path                  <- ""
  
  # =========================================================================== #
  # Set Transient as 0 so that we only simulate steady-state
  MECHA_inputs_list$Hormones$param$Time_information$Transient_sim$value           <- as.integer(0)
  # =========================================================================== #
  # Set observation point
  MECHA_inputs_list$Geometry$param$Printrange$Print_layer$value                   <- iLayer
  # =========================================================================== #
  # Paraview outputs
  MECHA_inputs_list$General$properties$Paraview$WallFlux                          <- 1
  MECHA_inputs_list$General$properties$Paraview$MembraneFlux                      <- 1
  MECHA_inputs_list$General$properties$Paraview$PlasmodesmataFlux                 <- 1
  # =========================================================================== #
  # PLASMODESMATA HYDRAULIC CONDUCTANCE
  MECHA_inputs_list$Hydraulics$param$Kplrange$Kpl$value                           <- 3.5e-5              
  MECHA_inputs_list$Hydraulics$param$Kplrange$Kpl$PCC_factor                      <- as.double(1)        
  MECHA_inputs_list$Hydraulics$param$Kplrange$Kpl$PPP_factor                      <- as.double(1)
  MECHA_inputs_list$Hydraulics$param$Kplrange$Kpl$PST_factor                      <- as.double(1)
  MECHA_inputs_list$Hydraulics$param$Kplrange$Kpl$endo_in_factor                  <- as.double(10)
  MECHA_inputs_list$Hydraulics$param$Kplrange$Kpl$endo_out_factor                 <- as.double(1e-4) 
  # =========================================================================== #
  # PLASMODESMATA FREQUENCY
  MECHA_inputs_list$Hydraulics$param$Fplxheight_stele_sieve$value                 <- as.double(35000)  # Default : 350000
  MECHA_inputs_list$Hydraulics$param$Fplxheight_stele_comp$value                  <- as.double(35000)  # Default : 350000
  # =========================================================================== #
  # CELL WALL CONDUCTIVITIES
  MECHA_inputs_list$Hydraulics$param$kw_barrier_range$kw_barrier$Casp             <- as.double(1e-8)
  MECHA_inputs_list$Hydraulics$param$kw_barrier_range$kw_barrier$Sub_out          <- as.double(1e-16)
  MECHA_inputs_list$Hydraulics$param$kw_barrier_range$kw_barrier$Sub_in           <- as.double(1e-16)
  # =========================================================================== #
  # SET BOUNDARY CONDITIONS
  MECHA_inputs_list$BC$properties$BC_xyl_range$BC_xyl[[2]][["flowrate_prox"]]     <- 0.005                # Xylem BC as flowrate 
  MECHA_inputs_list$BC$properties$BC_sieve_range$BC_sieve[[2]][["osmotic"]]       <- as.double(-1.26e4)   # Phloem BC as osmotic (-1.26e4) or not ('nan')
  MECHA_inputs_list$BC$properties$Psi_cell_range$Psi_cell[[2]][["Os_hetero"]]     <- as.integer(1)        # other cells as osmotic (1) or not (0)
  # =========================================================================== #
  # ELONGATION RATE
  MECHA_inputs_list$BC$properties$Elong_cell_range$Elong_cell[[2]][["midpoint_rate"]] <- as.double(0.034) # Ellongation rate. Set 0 if no.
  
  return(MECHA_inputs_list)
}

# ============================================================================ #
# UPLOAD NEW ANATOMY
# ============================================================================ #
# Copy and paste anatomy to MECHA directory
MECHA.upload_anatomy <- function(filename,
                                 filepath,
                                 path_MECHA, 
                                 cellsetdata = "cellsetdata/",
                                 file_out = "current_root.xml"
){
  # Check if file is XML
  
  file.remove(paste0(path_MECHA, cellsetdata, file_out))
  
  file.copy(paste0(filepath, filename), 
            paste0(path_MECHA, cellsetdata))
  
  file.rename(paste0(path_MECHA, cellsetdata, filename), 
              paste0(path_MECHA, cellsetdata, file_out)
  )
  
}

# ============================================================================ #
# RUN MECHA
#============================================================================= # 

MECHA_get_results <- function(output_path)
  
  {
  
  list_files <- list.files(path = output_path, pattern = ".txt")
  
  Conds <- tibble()
  for(i in seq(1, length(list_files))){
    
    if(list_files[i] == "position.txt"){
      next
    }
    
    macro_prop <- read.delim(paste0(output_path, list_files[i]))
    
    kr         <- as.double(strsplit(macro_prop[8,], " ")[[1]][4])
    Kx         <- as.double(strsplit(macro_prop[7,], " ")[[1]][5])
    perimeter  <- as.double(strsplit(macro_prop[6,], " ")[[1]][3])
    
    # barrier    <- unlist(strsplit(list_files[i], split = ","))[2]
    # barrier    <- unlist(strsplit(barrier, split = ".txt"))[1]
    # barrier    <- paste0("b", barrier)
    
    barrier    <- unlist(strsplit(list_files[i], split = ","))[1]
    barrier    <- unlist(strsplit(barrier, split = "_"))[3]
    barrier    <- paste0("b", barrier)
    
    Conds_temp <- tibble(Barrier   = barrier,
                         kr        = kr,
                         Kx        = Kx,
                         perimeter = perimeter)
    
    Conds <- rbind(Conds, Conds_temp)
  }
  
  return(Conds)
}
