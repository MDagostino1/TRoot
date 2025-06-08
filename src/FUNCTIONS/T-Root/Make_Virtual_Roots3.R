#' Generates virtual roots based on the regression table made with Make_Regression(anatomy2). Generates random nodes along the root.
#'
#' @param n_nodes Number of nodes per virtual root
#' @param n_roots Number of virtual roots to generate
#' @param method Method to generate random. Alpha for a percentage, Sigma for the SD.
#' @param alpha Fraction of randomness to add. Only usefull when method == "alpha".
#' @export
#'

Make_Virtual_Roots3 <- function(Parameters = Parameters,
                                n_nodes = 5,
                               n_roots = 10,
                               method = "alpha",
                               alpha = 0.25){
  
  # Quality control
  if(is.null(n_nodes)){
    warning(paste0("Please enter a n_nodes."))
    return(NULL)
  }
  
  if(is.null(n_roots)){
    warning(paste0("Please enter a n_roots."))
    return(NULL)
  }
  
  if((method %in% c("alpha", "sigma")) == F){
    warning(paste0("Please set a correct method : alpha for a percentage or sigma for the SD"))
    return(NULL)
  }
  
  if((alpha < 0) | (alpha > 1)){
    warning(paste0("alpha must be between 0 and 1"))
    return(NULL)
  }
  
  
  #===============================================================================
  # Set variables
  # n_nodes = 3            # number of nodes per virtual root
  # n_roots = 3           # number of virtual roots to generate
  SG_age <- 7              # age of SG appearance. This will set phloem proportion to zero for each younger cross section
  #===============================================================================
  
  Virtual_Roots <- data.frame()
  X_vec <- seq(from = 0, to = 30, by = 30/n_nodes) # Calculation of X
  # xylem_mean_size <- as.numeric((anatomy2 %>% filter(param_id == "xylem_mean_size"))$value)  # for xylem_max_size, which is calculated from a regression of xylem_mean_size
  
  Params <- Parameters[c("name", "type", "param_id")]
  Params <- Params %>% filter(name != "secondarygrowth" & name != "randomness" & name != "aerenchyma" & name != "planttype" & type != "order" & type != "barrier")
  
  # Initiate loop for each param
  # NB : principle of the loop : Since all combinations of param_name and param_type do not exist, 
  # We first take a subset of Regressions with all parameters (-> Params), 
  # and then we loop per line of this subset, to avoid computing every combinations.
    
    #---------------------------------------------------------------------------
    # LOOP FOR ROOTS
    for(i in seq(1:n_roots)){
    #---------------------------------------------------------------------------
      
      for (c in seq(1:length(Params[c("name","type")]$name))) {
        
        #===============================================================================
        name_temp <- Params[c("name","type")][c,][,1]
        type_temp <- Params[c("name","type")][c,][,2]
        #===============================================================================
      
        # cat("Computing root : ", i, " | name : ", name_temp, " | type : ", type_temp, "\n")
        # flush.console()
        
        function_temp <- Params["param_id"][c,]
        
        if(function_temp == "endodermis_n_layers" | function_temp == "exodermis_n_layers" | function_temp == "epidermis_n_layers" | function_temp == "pericycle_n_layers"){
          Y <- 1
        }else{
          Y <- c()
          # ----------------------------------------------------------------------
          # LOOP FOR NODES
          # ----------------------------------------------------------------------
          for (x_i in X_vec) {
            # print(X)
            # We compute the Y value of the parameter at that node, using the reg function
            y_i <- get(function_temp)(x_i)
            # And we store in the Y vec
            Y <- c(Y, y_i)
          }  
          
        }
        
        
      # ----------------------------------------------------------------------
      #=========================================================================
      
      # save root in root_list
      temp_df <- data.frame(name = name_temp,
                            type = type_temp,
                            root = paste0("root", i), 
                            x = X_vec,
                            y = Y)
      Virtual_Roots <- rbind(Virtual_Roots, temp_df)
      
      # remove temp variables
      # rm(name_temp, type_temp, Y)
      
      
      
      }
  }
  
  # In order to differentiate primary and secondary growth, we use phloem proportion
  # Phloem proportion will be set to zero if the age is younger than SG appearance
  
  # print("Virtual roots generation done.")
  
  return(Virtual_Roots)
}
