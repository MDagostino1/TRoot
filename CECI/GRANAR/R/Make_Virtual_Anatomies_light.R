#' Runs only GRANAR on virtual roots and returns conductivities per virtual roots and per node.
#' GRANAR anatomies are stored in -Anatomies/Virtual_Anatomies/- folder
#' Require granar functions
#'
#' @param Virtual_Roots Table of the GRANAR params per virtual root and per node
#' @param MECHA_path Directory to the MECHA python file
#' @param output_path Directory to the MECHA results file
#' @export
#'


Make_Virtual_Anatomies_light <- function(Virtual_Roots,
                                   MECHA_path = "python E:/Exp1-09_01_23/MECHA/MECHAv4_septa_temp_V2.py",
                                   output_path = "MECHA/Projects/granar/out/Tomato/Root/Project_Test/results/",
                                   anatomy_path = "Anatomies/Virtual_Anatomies/")
  {
  
  # estimated_time <- length(unique(Virtual_Roots$root)) * length(unique(Virtual_Roots$x)) * 2.6
  # print(paste0("Estimated time : ", estimated_time, " min."))
  # t0 <- Sys.time()
  
  # Load templates
  # template1 <- read_param_xml("GRANAR/R/template.xml") # for primary growth transformation
  template <- read_param_xml("GRANAR/R/template.xml")    # for adding the rest of the data to the params
  VR_conductivities <- data.frame()
  # success <- c()
  
  # iteration_times <- c()
  
  #===============================================================================
  #===============================================================================
  
  print("Initializing loop...")
  for(temp_root in unique(Virtual_Roots$root)){
  # Initiate loop for roots
    
    # print(temp_root)
    
    # initiate loop for node
    for(temp_x in unique(Virtual_Roots$x)){
      
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
                
                myplot <- plot_anatomy(sim1)
                ggexport(myplot, filename=paste0(anatomy_path, Sys.Date(), "_", temp_root,"_",temp_x,".pdf"), plot = image)
                
                print("     Success of GRANAR execution.")
          
                # #===========================================================================
                # # Run MECHA
                # #===========================================================================
                # # Change current_root
                # print("Changing current root...")
                # write_anatomy_xml(sim1, "MECHA/cellsetdata/current_root.xml")
                # # Run MECHA
                # print("Launching MECHA...")
                # py_run_file("MECHA/MECHAv4_septa_temp_V2.py")
                # # system(paste0("python ",MECHA_path))
                # 
                # output_0 <- read.delim(paste0(output_path, "Macro_prop_0,0.txt"))
                # output_1 <- read.delim(paste0(output_path, "Macro_prop_1,1.txt"))
                # output_2 <- read.delim(paste0(output_path, "Macro_prop_2,2.txt"))
                # output_3 <- read.delim(paste0(output_path, "Macro_prop_3,3.txt"))
                # output_4 <- read.delim(paste0(output_path, "Macro_prop_4,4.txt"))
                # output_5 <- read.delim(paste0(output_path, "Macro_prop_5,5.txt"))
                # output_6 <- read.delim(paste0(output_path, "Macro_prop_6,6.txt"))
                # output_7 <- read.delim(paste0(output_path, "Macro_prop_7,7.txt"))
                # #===============================================================================
                # Kx_0 <- as.double(strsplit(output_0[7,], " ")[[1]][5])
                # Kx_1 <- as.double(strsplit(output_1[7,], " ")[[1]][5])
                # Kx_2 <- as.double(strsplit(output_2[7,], " ")[[1]][5])
                # Kx_3 <- as.double(strsplit(output_3[7,], " ")[[1]][5])
                # Kx_4 <- as.double(strsplit(output_4[7,], " ")[[1]][5])
                # Kx_5 <- as.double(strsplit(output_5[7,], " ")[[1]][5])
                # Kx_6 <- as.double(strsplit(output_6[7,], " ")[[1]][5])
                # Kx_7 <- as.double(strsplit(output_7[7,], " ")[[1]][5])
                # #===============================================================================
                # Kr_0 <- as.double(strsplit(output_0[8,], " ")[[1]][4])
                # Kr_1 <- as.double(strsplit(output_1[8,], " ")[[1]][4])
                # Kr_2 <- as.double(strsplit(output_2[8,], " ")[[1]][4])
                # Kr_3 <- as.double(strsplit(output_3[8,], " ")[[1]][4])
                # Kr_4 <- as.double(strsplit(output_4[8,], " ")[[1]][4])
                # Kr_5 <- as.double(strsplit(output_5[8,], " ")[[1]][4])
                # Kr_6 <- as.double(strsplit(output_6[8,], " ")[[1]][4])
                # Kr_7 <- as.double(strsplit(output_7[8,], " ")[[1]][4])
                # #===============================================================================
                # peri <- as.double(strsplit(output_0[6,], " ")[[1]][3])
                # print("     Success of MECHA execution. Saving data...")
                # 
                # # Save conductivities
                # Temp_K <- data.frame(Root = temp_root,
                #                      X = temp_x,
                #                      Barrier = c("b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7"),
                #                      Kr = c(Kr_0, Kr_1, Kr_2, Kr_3, Kr_4, Kr_5, Kr_6, Kr_7),
                #                      Kx = c(Kx_0, Kx_1, Kx_2, Kx_3, Kx_4, Kx_5, Kx_6, Kx_7),
                #                      perimeter = peri)
                # VR_conductivities <- rbind(VR_conductivities, Temp_K)
                # 
                # success <- c(success, paste0(temp_root, "_", temp_x))
                
                
                
                # it_time <- Sys.time() - t1
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
  
  # t1 <- Sys.time()
  print("----------------- End of computing. See you next time!. -----------------")
  
  # print("Total computing time :")
  # print(t1 - t0)
  # print("Mean iteration time :")
  # print(mean(iteration_times))
  
  # return(VR_conductivities)
  
} # end of function