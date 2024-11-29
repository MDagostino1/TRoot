#' Runs GRANAR and MECHA on virtual roots and returns conductivities per virtual roots and per node.
#' GRANAR anatomies are stored in -Anatomies/Virtual_Anatomies/- folder
#' Require granar functions
#'
#' @param Virtual_Roots Table of the GRANAR params per virtual root and per node
#' @param MECHA_path Directory to the MECHA python file
#' @param output_path Directory to the MECHA results file
#' @export
#'


Make_Virtual_Anatomies_G <- function(Virtual_Roots,
                                     template = "Data/template.xml",
                                     export_path_xml,
                                     export_path_pdf
                                     )
  {
  t0 <- Sys.time()
  library(ggpubr)

  # Load templates
  # template1 <- read_param_xml_old("GRANAR/R/template.xml") # for primary growth transformation
  template <- read_param_xml(template)    # for adding the rest of the data to the params

  iteration_times <- c()
  
  #===============================================================================
  #===============================================================================
  done_list <- list.files(export_path_xml, pattern = ".xml")
  
  print("Initializing loop...")
  for(temp_root in unique(Virtual_Roots$root)){
  # Initiate loop for roots

    # initiate loop for node
    for(temp_x in unique(Virtual_Roots$x)){
        
      name_temp <- paste0(temp_root, "_", temp_x)
      
      if(paste0(name_temp, ".xml") %in% done_list){
        print(paste0(temp_root, "_", temp_x, " has already been simulated, skipping iteration"))
        next
      }
      
      # Check if it has already been simulated
      

      # Delete temp_files
      print("Deleting temp files...")
      rm(temp_param, temp_param1, sim1, Temp_K, output_0, output_1, output_5, output_6, Kx_0, Kx_1, Kx_5, Kx_6, Kr_0, Kr_1, Kr_5, Kr_6) # To avoid memory issues

      print(paste0("Root : ", temp_root, " | node : ",temp_x))

      # Create the param file
      temp_VR <- Virtual_Roots %>% filter(root == temp_root, x == temp_x)
      temp_VR <- temp_VR[,c("name", "type", "y")]
      temp_VR <- dplyr::rename(temp_VR,"value" = "y")

      temp_param <- merge(template, temp_VR, by = c("name", "type"), all.x = T)
      temp_param$value.y <- ifelse(!is.na(temp_param$value.y), temp_param$value.y, temp_param$value.x)
      temp_param <- temp_param[, c("name", "type", "value.y")]
      colnames(temp_param)[3] <- "value"

      temp_param$value[temp_param$name == "planttype" & temp_param$type == "param"] = 2

      # initiate try
      tryCatch(
        expr= {
                #===========================================================================
                # Run GRANAR
                #===========================================================================
                print("Initializing GRANAR...")
                t1 <- Sys.time()
                print(t1)

                # if primary growth (condition = null phloem proportion)
                if(temp_param$value[temp_param$name == "phloem" & temp_param$type == "proportion"] == 0){
                  # Primary growth
                  temp_param$value[temp_param$name == "secondarygrowth"] = 0
                } else{
                  # Secondary growth
                  temp_param$value[temp_param$name == "secondarygrowth"] = 1
                } # end of else


                sim1 <- create_anatomy(parameters = temp_param, verbatim = F)
                print("Launching new GRANAR...")

                # Saving root
                print("Saving root anatomy XML...")
                write_anatomy_xml(sim1, paste0(export_path_xml,name_temp,".xml"))
                
                myplot <- plot_anatomy(sim1)
                ggexport(myplot, filename=paste0(export_path_pdf, name_temp,".pdf"), plot = image)
                
                it_time <- Sys.time() - t1
                # iteration_times <- c(iteration_times, it_time)
                # print(paste0("Iteration time : ", it_time, " minutes"))
                print("==========================================================")
                print("==========================================================")

        }, # end of tryCatch expression
        error=function(e){
          message("ERROR : \n")
          message(e)
          }
        )# end of tryCatch

      } # end of for temp_x

    } # end of for temp_root

  t1 <- Sys.time()
  print("----------------- End of computing. See you next time!. -----------------")

  print("Total computing time :")
  print(t1 - t0)
  print("Mean iteration time :")
  print(mean(iteration_times))

} # end of function
