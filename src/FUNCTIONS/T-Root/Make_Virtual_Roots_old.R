#' Generates virtual roots based on the regression table made with Make_Regression(anatomy2). Generates fixed nodes along the root.
#'
#' @param Regressions Table with the equation of the regression and sigma for each param
#' @param n_nodes Number of nodes per virtual root
#' @param n_roots Number of virtual roots to generate
#' @param method Method to generate random. Alpha for a percentage, Sigma for the SD.
#' @param alpha Fraction of randomness to add. Only usefull when method == "alpha".
#' @export
#'

Make_Virtual_Roots_old <- function(Regressions,
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
  # n_nodes = 5            # number of nodes per virtual root
  # n_roots = 10           # number of virtual roots to generate
  SG_age <- 7              # age of SG appearance. This will set phloem proportion to zero for each younger cross section
  #===============================================================================
  
  Virtual_Roots <- data.frame()
  X <- seq(from = 0, to = 30, by = 30/n_nodes) # Calculation of X
  
  Regressions_E <- merge(Parameters[c("name", "type", "param_id")], Regressions)
  
  # print("Initializing...")
  Reg2 <- Regressions_E %>% filter(reg_param == "Fx")
  
  # Initiate loop for each param
  # NB : principle of the loop : Since all combinations of param_name and param_type do not exist, 
  # We first take a subset of Regressions with all parameters (-> Reg2), 
  # and then we loop per line of this subset, to avoid computing every combinations.
  for (c in seq(1:length(Reg2[c("name","type")]$name))) {
    
    #===============================================================================
    name_temp <- Reg2[c("name","type")][c,][,1]
    type_temp <- Reg2[c("name","type")][c,][,2]
    #===============================================================================
    
    # print(paste0("Name : ", name_temp, "| Type = ", type_temp))
    
    #===============================================================================
    # Load regression equation and sigma
    reg_eq <- Regressions$value[Regressions$name == name_temp & Regressions$type == type_temp & Regressions$reg_param == "Fx"]
    reg_sigma <- as.numeric(Regressions$value[Regressions$name == name_temp & Regressions$type == type_temp & Regressions$reg_param == "sigma"])
    #===============================================================================
    
    # Generate regression points
    Y <- round(as.numeric(eval(parse(text = reg_eq))), 4)
    
    # print(paste0("Generating ", n, " roots..."))
    
    # initiate loop of roots
    for(i in seq(1:n_roots)){                    
      
      # print(paste0("Generating root", i))
      
      root_temp <- c()
      
      # initiate loop of nodes in root
      for(j in Y){
        
        #===============================================================================
        # Generate k = a value of a param at the node j of the root i
        if(method == "alpha"){
          k <- rnorm(1, mean = j, sd = alpha*j)
          
        }else if(method == "sigma"){
          k <- rnorm(1, mean = j, sd = reg_sigma)
          
        } else{
          warning(message = paste0("Incorrect method"))
          return(NULL)
        }
        
        # Cases where it needs to be rounded
        if(type_temp %in% c("n_files", "n_layers")){
          k <- round(k)
        }
        
        # Case for phloem proportion to set to zero
        temp_age <- X[which(Y == j)]
        if(type_temp == "proportion" & temp_age < SG_age){
          k <- 0
        }
        
        root_temp <- c(root_temp, k)
        #===============================================================================
        
        # print(paste0("     j : ", j, "| k :", k))  # j = initial value and k = random value
      }
      
      # save root in root_list
      temp_df <- data.frame(name = name_temp,
                            type = type_temp,
                            root = paste0("root", i), 
                            x = X,
                            y = root_temp)
      
      Virtual_Roots <- rbind(Virtual_Roots, temp_df)
    }
    
  }
  
  # In order to differentiate primary and secondary growth, we use phloem proportion
  # Phloem proportion will be set to zero if the age is younger than SG appearance
  
  # print("Virtual roots generation done.")
  
  return(Virtual_Roots)
}