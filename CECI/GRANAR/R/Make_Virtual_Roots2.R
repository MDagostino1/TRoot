#' Generates virtual roots based on the regression table made with Make_Regression(anatomy2). Generates random nodes along the root.
#'
#' @param Regressions Table with the equation of the regression and sigma for each param
#' @param n_nodes Number of nodes per virtual root
#' @param n_roots Number of virtual roots to generate
#' @param method Method to generate random. Alpha for a percentage, Sigma for the SD.
#' @param alpha Fraction of randomness to add. Only usefull when method == "alpha".
#' @export
#'

Make_Virtual_Roots2 <- function(Regressions,
                               n_nodes = 5,
                               n_roots = 10,
                               method = "alpha",
                               alpha = 0.25){
  
  # Quality control
  if(is.null(Regressions)){
    warning(paste0("No input file. Please enter a correct regressions file."))
    return(NULL)
  }
  
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
  # X_vec <- seq(from = 0, to = 30, by = 30/n_nodes) # Calculation of X
  xylem_mean_size <- as.numeric((anatomy2 %>% filter(param_id == "xylem_mean_size"))$value)  # for xylem_max_size, which is calculated from a regression of xylem_mean_size
  
  Regressions_E <- merge(Parameters[c("name", "type", "param_id")], Regressions)
  
  # print("Initializing...")
  Reg2 <- Regressions_E %>% filter(reg_param == "Fx")
  
  # Initiate loop for each param
  # NB : principle of the loop : Since all combinations of param_name and param_type do not exist, 
  # We first take a subset of Regressions with all parameters (-> Reg2), 
  # and then we loop per line of this subset, to avoid computing every combinations.
 
    
    
    #---------------------------------------------------------------------------
    # LOOP FOR ROOTS
    for(i in seq(1:n_roots)){
    #---------------------------------------------------------------------------
      
      # Generate X vector
      X_vec <- runif(n = n_nodes, min = 0, max = 30)
      
      for (c in seq(1:length(Reg2[c("name","type")]$name))) {
        
        #===============================================================================
        name_temp <- Reg2[c("name","type")][c,][,1]
        type_temp <- Reg2[c("name","type")][c,][,2]
        #===============================================================================
        
        # print(paste0("Name : ", name_temp, "| Type = ", type_temp))
        
        # Load regression equation and sigma
        reg_eq <- Regressions_E$value[Regressions_E$name == name_temp & Regressions_E$type == type_temp & Regressions_E$reg_param == "Fx"]
        reg_sigma <- as.numeric(Regressions_E$value[Regressions_E$name == name_temp & Regressions_E$type == type_temp & Regressions_E$reg_param == "sigma"])
      
      #=========================================================================
      # Sigma method
      #=========================================================================
      if(method == "sigma"){
        
        Y <- c()
        
        # ----------------------------------------------------------------------
        # LOOP FOR NODES
        # ----------------------------------------------------------------------
        for (X in X_vec) {
          # print(X)
          
          # First we generate a random residual, following a normal distribution around 0 and with sd = reg_sigma
          sd <- rnorm(1, mean = 0, sd = reg_sigma)
          # Next we compute the Y value of the parameter at that node, using the reg equation
          Y_temp <- round(as.numeric(eval(parse(text = reg_eq))), 4)
          
          # Case for phloem proportion
          if(type_temp == "proportion" & X < SG_age){
            Y_temp <- 0
          }
          
          # And we store in the Y vec
          Y <- c(Y, Y_temp)
        }
        # ----------------------------------------------------------------------
        
        # we can delete the temp values :
        # rm(sd, X, Y_temp)
      
      #=========================================================================
      # Alpha method
      #=========================================================================  
      } else if(method == "alpha"){
        print("not yet implemented")
        
        stop()
        next
        
      }else{
        warning(message = paste0("Incorrect method"))
        return(NULL)
      }
      
      #=========================================================================
      # Cases when it needs to be rounded
      if(type_temp %in% c("n_files", "n_layers")){
        Y <- round(Y)
      }
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
