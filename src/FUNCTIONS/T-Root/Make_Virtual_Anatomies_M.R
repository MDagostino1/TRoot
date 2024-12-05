#' Runs GRANAR and MECHA on virtual roots and returns conductivities per virtual roots and per node.
#' GRANAR anatomies are stored in -Anatomies/Virtual_Anatomies/- folder
#' Require granar functions
#'
#' @param Virtual_Roots Table of the GRANAR params per virtual root and per node
#' @param MECHA_path Directory to the MECHA python file
#' @param output_path Directory to the MECHA results file
#' @export
#'

Make_Virtual_Anatomies_M <- function(Virtual_Roots,
                                     anatomy_path,
                                   MECHA_path = "MECHA/MECHAv4_TRoot.py",
                                   output_path,
                                   conds_path,
                                   done_path,
                                   hydraulic_path,
                                   AQP_list = c(0.00043))
  {

  t0 <- Sys.time()
  success <- c()
  iteration_times <- c()
  
  done_list <- list.files(done_path, pattern = ".csv")  
  anatomy_list <- list.files(anatomy_path, pattern = ".xml")
  
  #===============================================================================
  #===============================================================================
  
  for(temp_root in unique(Virtual_Roots$root)){

    # initiate loop for node
    for(temp_x in unique(Virtual_Roots$x)){
      
      name_file <- paste0(temp_root,"_", temp_x, ".xml")
      
      # Check if there is a GRANAR anatomy
      if(name_file %in% anatomy_list){
        print("Launching MECHA")
      }else{
        print("Skipping File")
        next
      }
      
      # Check if it has already been simulated or not
      if(paste0(temp_root, "_", temp_x, ".csv") %in% done_list){
        print(paste0(temp_root, "_", temp_x, " has already been simulated, skipping iteration"))
        next
      }
            
      Temp_Conds <- tibble()
      
      for(temp_AQP in AQP_list){
        
        print(paste0("Root : ", temp_root, " | node : ",temp_x, " | kAQP : ",temp_AQP))
        
        # Delete current root if it exist
        if(file.exists("MECHA/cellsetdata/current_root.xml")){
          file.remove("MECHA/cellsetdata/current_root.xml")
        }
        
        # Moving file to MECHA
        file.copy(paste0(anatomy_path, name_file), 
                  "MECHA/cellsetdata/")
        
        file.rename(paste0("MECHA/cellsetdata/", name_file),
                    "MECHA/cellsetdata/current_root.xml")
        
        # ======================================================================
        # Change AQP value in file
        hydraulic_file <- read_xml(hydraulic_path)
        xml_find_all(hydraulic_file, "//kAQP") %>% 
          xml_set_attr("value", temp_AQP)
        write_xml(hydraulic_file, hydraulic_path)
        # ======================================================================
        
        # initiate try
        tryCatch(
          expr= {
            
            #===========================================================================
            # Run MECHA
            #===========================================================================
            # Change current_root
            print("Changing current root...")
            
            # Run MECHA
            print("Launching MECHA...")
            py_run_file(MECHA_path)
            # system(paste0("python ",MECHA_path))
            
            output_0 <- read.delim(paste0(output_path, "Macro_prop_0,0.txt"))
            output_1 <- read.delim(paste0(output_path, "Macro_prop_1,1.txt"))
            output_2 <- read.delim(paste0(output_path, "Macro_prop_2,2.txt"))
            output_3 <- read.delim(paste0(output_path, "Macro_prop_3,3.txt"))
            output_4 <- read.delim(paste0(output_path, "Macro_prop_4,4.txt"))
            output_5 <- read.delim(paste0(output_path, "Macro_prop_5,5.txt"))
            output_6 <- read.delim(paste0(output_path, "Macro_prop_6,6.txt"))
            output_7 <- read.delim(paste0(output_path, "Macro_prop_7,7.txt"))
            output_8 <- read.delim(paste0(output_path, "Macro_prop_8,8.txt"))
            output_9 <- read.delim(paste0(output_path, "Macro_prop_9,9.txt"))
            #===============================================================================
            Kx_0 <- as.double(strsplit(output_0[7,], " ")[[1]][5])
            Kx_1 <- as.double(strsplit(output_1[7,], " ")[[1]][5])
            Kx_2 <- as.double(strsplit(output_2[7,], " ")[[1]][5])
            Kx_3 <- as.double(strsplit(output_3[7,], " ")[[1]][5])
            Kx_4 <- as.double(strsplit(output_4[7,], " ")[[1]][5])
            Kx_5 <- as.double(strsplit(output_5[7,], " ")[[1]][5])
            Kx_6 <- as.double(strsplit(output_6[7,], " ")[[1]][5])
            Kx_7 <- as.double(strsplit(output_7[7,], " ")[[1]][5])
            Kx_8 <- as.double(strsplit(output_8[7,], " ")[[1]][5])
            Kx_9 <- as.double(strsplit(output_9[7,], " ")[[1]][5])
            #===============================================================================
            Kr_0 <- as.double(strsplit(output_0[8,], " ")[[1]][4])
            Kr_1 <- as.double(strsplit(output_1[8,], " ")[[1]][4])
            Kr_2 <- as.double(strsplit(output_2[8,], " ")[[1]][4])
            Kr_3 <- as.double(strsplit(output_3[8,], " ")[[1]][4])
            Kr_4 <- as.double(strsplit(output_4[8,], " ")[[1]][4])
            Kr_5 <- as.double(strsplit(output_5[8,], " ")[[1]][4])
            Kr_6 <- as.double(strsplit(output_6[8,], " ")[[1]][4])
            Kr_7 <- as.double(strsplit(output_7[8,], " ")[[1]][4])
            Kr_8 <- as.double(strsplit(output_8[8,], " ")[[1]][4])
            Kr_9 <- as.double(strsplit(output_9[8,], " ")[[1]][4])
            #===============================================================================
            peri <- as.double(strsplit(output_0[6,], " ")[[1]][3])
            print("     Success of MECHA execution. Saving data...")
            
            # Save conductivities
            Temp_K <- data.frame(Root = temp_root,
                                 X = temp_x,
                                 Barrier = c("b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9"),
                                 Kr = c(Kr_0, Kr_1, Kr_2, Kr_3, Kr_4, Kr_5, Kr_6, Kr_7, Kr_8, Kr_9),
                                 Kx = c(Kx_0, Kx_1, Kx_2, Kx_3, Kx_4, Kx_5, Kx_6, Kx_7, Kx_8, Kx_9),
                                 kAQP = temp_AQP,
                                 perimeter = peri)
            
            # write.csv(Temp_K, file = paste0(result_path, temp_root,"_",temp_x,"_", temp_AQP,".csv"), row.names = F)
            Temp_Conds <- rbind(Temp_Conds, Temp_K)
            
            it_time <- Sys.time() - t0
            iteration_times <- c(iteration_times, it_time)
            print(paste0("Iteration time : ", it_time))
            print("==========================================================")
            print("==========================================================")
            
          }, # end of tryCatch expression
          error=function(e){
            message("ERROR \n")
            message(e)
          }
        )# end of tryCatch
        
      } # end of AQP
      
      result_name <- paste0(temp_root, "_", temp_x)
      
      # Write Conductivity results
      write.csv(Temp_Conds, file = paste0(conds_path, temp_root,"_",temp_x,".csv"), row.names = F)
      
      # Write that the simulation has been done
      write.csv(tibble("done"), file = paste0(done_path, temp_root,"_",temp_x,".csv"), row.names = F)
      
      rm(Temp_Conds)
      
    } # end of temp_x
    
    
  } # end of temp_root

  t1 <- Sys.time()
  print("----------------- End of computing. See you next time!. -----------------")
  
  print("Total computing time :")
  print(t1 - t0)
      
  # return(VR_conductivities)
  }# end of function
