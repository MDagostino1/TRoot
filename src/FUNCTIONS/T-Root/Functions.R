#===============================================================================
#===============================================================================

extract_parenchyma <- function(parenchyma){

  # extract data
  max_size <- max(parenchyma$area)
  min_size <- min(parenchyma$area)
  mean_size <- mean(parenchyma$area)

  cell_diameter <- 2*sqrt((parenchyma$area)/pi)/1000  # mean radius = mean(sqrt(A)/pi) and not the formula with mean area
  sd_size <-  sd(cell_diameter)
  mean_diameter <- mean(cell_diameter)


  # max_size_df <- data.frame(name = "stele", type = "max_size", value = max_size)
  # min_size_df <- data.frame(name = "stele", type = "min_size", value = min_size)
  # mean_size_df <- data.frame(name = "stele", type = "mean_size", value = mean_size)
  mean_diameter_df <- data.frame(name = "stele", type = "cell_diameter", value = mean_diameter)
  sd_size_df <- data.frame(name = "stele", type = "SD", value = sd_size)

  # store in a dataframe
  data_parenchyma <- rbind(mean_diameter_df,
                           sd_size_df)

  # return the df
  return(data_parenchyma)
}


#===============================================================================
#===============================================================================

extract_xylem <- function(xylem){

  # extract data
  n_files <- 2 # for tomato
  n_cells <- length(xylem$xylem)
  max_size <- max(xylem$area)
  min_size <- min(xylem$area)
  mean_size <- mean(xylem$area)
  order <- 1.5

  n_files_df <- data.frame(name = "xylem", type = "n_files", value = n_files)
  n_cells_df <- data.frame(name = "xylem", type = "n_cells", value = n_cells)
  max_size_df <- data.frame(name = "xylem", type = "max_size", value = sqrt(max_size/pi)*0.002)    # max_diameter -> mm
  # min_size_df <- data.frame(name = "xylem", type = "min_size", value = min_size)
  # mean_size_df <- data.frame(name = "xylem", type = "mean_size", value = mean_size)
  order_df <- data.frame(name = "xylem", type = "order", value = order)
  cell_diameter_df <- data.frame(name = "xylem", type = "cell_diameter", value = (sqrt(mean_size/pi)*2)/1000)

  # store in a dataframe
  data_xylem <- rbind(n_files_df,
                      n_cells_df,
                      max_size_df,
                      cell_diameter_df,
                      order_df)

  # return the df
  return(data_xylem)
}


#===============================================================================
#===============================================================================


extract_phloem <- function(phloem){

  # extract data

  # Size as area
  #max_size <- max(phloem$area)
  #min_size <- min(phloem$area)
  #mean_size <- mean(phloem$area)

  # Size as diameter
  mean_cell_diameter <- mean(phloem$length)/1000 # [nm]

  # max_size_df <- data.frame(name = "phloem", type = "max_size", value = max_size)
  # min_size_df <- data.frame(name = "phloem", type = "min_size", value = min_size)
  # mean_size_df <- data.frame(name = "phloem", type = "mean_size", value = mean_size)
  # cell_diameter_df <- data.frame(name = "phloem", type = "cell_diameter", value = mean_cell_diameter)

  # store in a dataframe
  phloem_df <- data.frame(name = "phloem",
                          type = c("cell_diameter", "order"),
                          value = c(mean_cell_diameter, 1.5))

  # return the df
  return(phloem_df)
}

#===============================================================================
#===============================================================================

extract_stele <- function(stele){

  # extract data
  totarea <- stele$area
  order <- 1.0

  totarea_df <- data.frame(name = "stele",
                           type = "totarea",
                           value = totarea)

  order_df <- data.frame(name = "stele", type = "order", value = order)

  # store in a dataframe
  data_stele <- rbind(order_df)

  # return the df
  return(data_stele)
}


#===============================================================================
#===============================================================================

extract_cortex <- function(cortex){

  cell_diameter <- mean(cortex$length)/1000
  order <- 4.0                         # by default

  data_cortex = data.frame(name = c("cortex", "cortex"),
                           type = c("cell_diameter", "order"),
                           value = c(cell_diameter, order))

  return(data_cortex)
}

#===============================================================================
#===============================================================================

extract_pericycle <- function(pericycle){

  cell_diameter <- mean(pericycle$length)/1000
  order <- 2.0
  n_layers <- 1.0

  cell_diameter_df <- data.frame(name = "pericycle", type = "cell_diameter", value = cell_diameter)
  order_df <- data.frame(name = "pericycle", type = "order", value = order)
  n_layers_df <- data.frame(name = "pericycle", type = "n_layers", value = n_layers)

  data_pericycle = rbind(cell_diameter_df,
                         order_df,
                         n_layers_df)

  return(data_pericycle)
}

#===============================================================================
#===============================================================================

extract_endodermis <- function(endodermis){

  cell_diameter <- mean(endodermis$length)/1000
  n_layers <- 1.0                    # by default
  order <- 3.0                         # by default

  data_endodermis = data.frame(name = c("endodermis", "endodermis", "endodermis"),
                               type = c("cell_diameter", "n_layers", "order"),
                               value = c(cell_diameter, n_layers, order))

  return(data_endodermis)
}

#===============================================================================
#===============================================================================

extract_epidermis <- function(epidermis){

  cell_diameter <- mean(epidermis$length)/1000
  n_layers <- 1.0                    # by default
  order <- 6.0                         # by default

  data_epidermis = data.frame(name = c("epidermis", "epidermis", "epidermis"),
                              type = c("cell_diameter", "n_layers", "order"),
                              value = c(cell_diameter, n_layers, order))


  return(data_epidermis)
}


#===============================================================================
#===============================================================================

extract_exodermis <- function(exodermis){

  cell_diameter <- mean(exodermis$length)/1000
  n_layers <- 1.0                    # by default
  order <- 5.0                         # by default
  
  data_exodermis = data.frame(name = c("exodermis", "exodermis", "exodermis"),
                              type = c("cell_diameter", "n_layers", "order"),
                              value = c(cell_diameter, n_layers, order))
  
  return(data_exodermis)
}


#===============================================================================
#===============================================================================

extract_barriers <- function(barriers){
  
  b_endo <- data_frame(name = "endodermis", type = "barrier", value = barriers$stage[barriers$type == "endodermis"])
  b_exo <- data_frame(name = "exodermis", type = "barrier", value = barriers$stage[barriers$type == "exodermis"])
  
  data_barriers <- rbind(b_endo, b_exo)
  
  return(data_barriers)
  
}


#===============================================================================
#===============================================================================

extract_layers <- function(layers){
  
  l_stele <- layers$layers[layers$name == "stele"]
  d_stele <- 2*layers$radius[layers$name == "stele"]/1000 # ?m -> mm
  l_cortex <- layers$layers[layers$name == "cortex"]
  r_cortex <- layers$radius[layers$name == "cortex"]
  phloem_layer <- layers$layers[layers$name == "phloem"]
  phloem_radius <- layers$radius[layers$name == "phloem"]/1000 # ?m -> mm
  
  # proportion of phloem in the stele
  phloem_prop <- phloem_radius/(d_stele/2)
  
  l_stele_df <- data.frame(name = "stele", type = "n_layers", value = l_stele)
  d_stele_df <- data.frame(name = "stele", type = "layer_diameter", value = d_stele)
  l_cortex_df <- data.frame(name = "cortex", type = "n_layers", value = l_cortex)
  #r_cortex_df <- data.frame(name = "cortex", type = "radius", value = r_cortex)                          # EXPORT CORTEX LAYER DIAMETER?????
  phloem_layer_df <- data.frame(name = "phloem", type = "n_layers", value = phloem_layer)
  phloem_prop_df <- data.frame(name = "phloem", type = "proportion", value = phloem_prop)
  
  data_layers <- rbind(l_stele_df,
                       d_stele_df,
                       l_cortex_df,
                       # r_cortex_df,
                       phloem_layer_df,
                       phloem_prop_df)
  
  return(data_layers)
}


#===============================================================================
#===============================================================================

extract_all <- function(path){
  # input : a path of xlsx file with quantification (ex : A01_S1_C01_Z04_01.xlsx)
  # output : a dataframe with the parameters of GRANAR
  
  # Basic parameters :
  b_params <- data.frame(name = c("secondarygrowth", "randomness", "planttype"),
                         type = c("param", "param", "param"),
                         value = c(1, 2, 2))
  
  # aerenchyma
  data_aerenchyma <- data.frame(name = c("aerenchyma", "aerenchyma"),
                                type = c("proportion", "n_files"),
                                value = c(0.0, 10.0))
  
  
  # quantified tissues
  xylem      <- na.omit(read_xlsx(path, sheet = "Xylem"))
  parenchyma <- na.omit(read_xlsx(path, sheet = "Parenchyma"))
  stele      <- na.omit(read_xlsx(path, sheet = "Stele"))
  cortex     <- na.omit(read_xlsx(path, sheet = "Cortex"))
  pericycle  <- na.omit(read_xlsx(path, sheet = "Pericycle"))
  endodermis <- na.omit(read_xlsx(path, sheet = "Endodermis"))
  epidermis  <- na.omit(read_xlsx(path, sheet = "Epidermis"))
  phloem     <- na.omit(read_xlsx(path, sheet = "Phloem"))
  # barriers   <- na.omit(read_xlsx(path, sheet = "Barriers"))
  layers     <- na.omit(read_xlsx(path, sheet = "Layers"))
  exodermis  <- na.omit(read_xlsx(path, sheet = "Exodermis"))
  
  # Extract information from subvariables
  data_xylem <- extract_xylem(xylem)
  data_parenchyma <- extract_parenchyma(parenchyma)
  data_phloem <- extract_phloem(phloem)
  data_stele <- extract_stele(stele)
  data_cortex <- extract_cortex(cortex)
  data_endodermis <- extract_endodermis(endodermis)
  data_epidermis <- extract_epidermis(epidermis)
  data_exodermis <- extract_exodermis(exodermis)
  data_pericycle <- extract_pericycle(pericycle)
  # data_barriers <- extract_barriers(barriers)
  data_layers <- extract_layers(layers)
  
  # Extract each variables
  data_all <- rbind(b_params,
                    data_aerenchyma,
                    data_xylem,
                    data_parenchyma,
                    data_phloem,
                    data_stele,
                    data_cortex,
                    data_endodermis,
                    data_exodermis,
                    data_epidermis,
                    data_pericycle,
                    # data_barriers,
                    data_layers)
  
  return(data_all)
}


#===============================================================================
#===============================================================================

extract_ids <- function(list, i){
  #_____________________________________________________________________________
  # @list = list of all xlsx files
  # @i    = iteration
  # this function will extract metadata from the name of the xlsx file
  # and generate a dataframe row :
  # id | plant_id | segment | cut | zoom | repetition
  #_____________________________________________________________________________
  
  # take the i string
  temp_string <- list[i]
  
  # split to list
  temp_string2 <- strsplit(temp_string, "_")
  
  # define params
  temp_id <- i
  temp_plant_id   <- temp_string2[[1]][1]
  temp_segment    <- temp_string2[[1]][2]
  temp_coloration <- temp_string2[[1]][3]
  temp_cut        <- temp_string2[[1]][4]
  temp_zoom       <- temp_string2[[1]][5]
  temp_repetition <- strsplit(temp_string2[[1]][6], ".xlsx")[[1]][1]
  
  # create dataframe
  temp_df = data.frame(id = temp_id,
                       plant_id = temp_plant_id,
                       segment = temp_segment,
                       coloration = temp_coloration,
                       cut = temp_cut,
                       zoom = temp_zoom,
                       repetition = temp_repetition)
  
  return(temp_df)
}


#===============================================================================
#===============================================================================

# Make a list of all files names
Merge_Quantifications <- function(q_path){
  
  anatomy_DB <- data.frame(cs_id = c(),
                           name = c(),
                           type = c(),
                           value = c())
  
  list <- list.files(path = q_path, pattern = "xlsx")
  
  for(k in seq(1:length(list))){
    path = paste0(q_path, list[k])
    
    # Generate the row of ids
    cs_id <- extract_ids(list, k)
    
    print(paste0("Initializing incorporation of data from cross section ", cs_id$plant_id, " , segment", cs_id$segment))
    
    # load file with the path
    file <- read_xlsx(path)
    sheets <- excel_sheets(path)
    
    # Process each sheet
    for(i in sheets){
      
      # print(i)
      
      if(i == "Layers"){
        #print("This is the layer")
        temp_file <- read_xlsx(path, sheet = i)
        
        for (p in seq(1:length(temp_file$id))) {
          
          temp_df_layer <- data.frame(name = temp_file$name[p],
                                      type = "nLayers",
                                      value = temp_file$layers[p])
          temp_df_layer2 <- merge(cs_id, temp_df_layer)
          anatomy_DB <- rbind(anatomy_DB, temp_df_layer2)
          
          temp_df_radius <- data.frame(name = temp_file$name[p],
                                       type = "radius",
                                       value = temp_file$radius[p])
          temp_df_radius2 <- merge(cs_id, temp_df_radius)
          anatomy_DB <- rbind(anatomy_DB, temp_df_radius2)
        }
        
      }
      
      else{
        temp_file <- read_xlsx(path, sheet = i)
        name = i
        
        # Process each params per sheet
        types = colnames(temp_file) # we take from 2 because the first column is the id
        
        for (j in seq(from = 2,
                      to = length(types),
                      by = 1)) {
          
          temp_df <- data.frame(value = temp_file[[j]])
          temp_df$name = i
          temp_df$type = types[j]
          # temp_df$measure_id = seq(from = 1,
          #                          to = length(temp_df$value),
          #                          by = 1)
          
          temp_df2 <- merge(cs_id, temp_df)
          anatomy_DB <- rbind(anatomy_DB, temp_df2)
          
        } # end of j for
      } # end of else
    } # end of i
  } # end of k for
  return(anatomy_DB)
} # end of function

#===============================================================================
#===============================================================================

transform_param <- function(param1, template){
  # For tomato, I needed to encode data in anaother way that Adrien, so this script transform my new-encoded data into old-encoded data.
  
  # template
  template2 <- template[c(1,2)]
  
  # merge template and actual data
  param2 <- merge(x = template2, y = param1, by = c("name", "type"), all.x=TRUE)
  
  # change values that are not compatible
  
  # In pack xylem, xylem max size = area, while max_size is length in ?m in old version
  # diameter = 2*((area/3.1415)^0.5)/1000
  xylem_area <- param1$value[param1$name == "xylem" & param1$type == "max_size"]
  param2$value[param2$name == "xylem" & param2$type == "max_size"] = 2*((xylem_area/3.1415)^0.5)/1000
  param2$value[param2$name == "xylem" & param2$type == "n_files"]  = 2      # always true for tomato
  
  param2$value[param2$name == "phloem" & param2$type == "n_files"] = 2      # always true for tomato
  
  param2$value[param2$name == "stele" & param2$type == "n_layers"]   = 1
  
  return(param2)
}

#===============================================================================
#===============================================================================

anatomy_plot <- function(Data = data,
                         Title = title){
  
  ggplot(data = Data,
         aes(x = age,
             y = value,
             color = plant_id)) +
    geom_point() +
    # geom_line() +
    
    # stat_summary(fun.y = median, geom = "line",
    #              aes(group = gen_id, color = gen_id),
    #              position = position_dodge(width = 0.9)) +
    
    #facet_wrap(~segment, ncol = 4) +
    theme_bw() +
    # scale_fill_manual(values = c("G" = "coral1", "A" =  "turquoise3")) +
    #geom_jitter(color="black", size=0.4, alpha=0.9) + # Add points
    
    theme(axis.line = element_line(color='black'),
          # Remove grid
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
          # Change font
          # text = element_text(family = "A"),
          # Remove x ticks
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          # Remove legend title
          #legend.title=element_blank()
          # Remove legend
          # legend.position="none"
          # Remove panel borders
          #panel.border = element_blank()
    ) +
    ggtitle(Title)
}

#===============================================================================
#===============================================================================

extract_conds <- function(output_path = "MECHA/Projects/granar/out/Tomato/Root/Project_Test/results/"){
  
  output_0 <- read.delim(paste0(output_path, "Macro_prop_0,0.txt"))
  output_1 <- read.delim(paste0(output_path, "Macro_prop_1,1.txt"))
  output_5 <- read.delim(paste0(output_path, "Macro_prop_5,2.txt"))
  output_6 <- read.delim(paste0(output_path, "Macro_prop_6,3.txt"))
  #===============================================================================
  Kx_0 <- as.double(strsplit(output_0[7,], " ")[[1]][5])
  Kx_1 <- as.double(strsplit(output_1[7,], " ")[[1]][5])
  Kx_5 <- as.double(strsplit(output_5[7,], " ")[[1]][5])
  Kx_6 <- as.double(strsplit(output_6[7,], " ")[[1]][5])
  #===============================================================================
  kr_0 <- as.double(strsplit(output_0[8,], " ")[[1]][4])
  kr_1 <- as.double(strsplit(output_1[8,], " ")[[1]][4])
  kr_5 <- as.double(strsplit(output_5[8,], " ")[[1]][4])
  kr_6 <- as.double(strsplit(output_6[8,], " ")[[1]][4])
  #===============================================================================
  peri <- as.double(strsplit(output_0[6,], " ")[[1]][3])
  #===============================================================================
  Conductivities <- data.frame(Barrier = c("b0", "b1", "b5", "b6"),
                               kr = c(kr_0, kr_1, kr_5, kr_6),
                               Kx = c(Kx_0, Kx_1, Kx_5, Kx_6),
                               perimeter = c(peri))
  
  return(Conductivities)
}

#===============================================================================
#===============================================================================

extract_conds2 <- function(output_path = "MECHA/Projects/granar/out/Tomato/Root/Project_Test/results/"){
  
  output_0 <- read.delim(paste0(output_path, "Macro_prop_0,0.txt"))
  output_1 <- read.delim(paste0(output_path, "Macro_prop_1,1.txt"))
  output_2 <- read.delim(paste0(output_path, "Macro_prop_2,2.txt"))
  output_3 <- read.delim(paste0(output_path, "Macro_prop_3,3.txt"))
  output_4 <- read.delim(paste0(output_path, "Macro_prop_4,4.txt"))
  output_5 <- read.delim(paste0(output_path, "Macro_prop_5,5.txt"))
  output_6 <- read.delim(paste0(output_path, "Macro_prop_6,6.txt"))
  output_7 <- read.delim(paste0(output_path, "Macro_prop_7,7.txt"))
  #===============================================================================
  Kx_0 <- as.double(strsplit(output_0[7,], " ")[[1]][5])
  Kx_1 <- as.double(strsplit(output_1[7,], " ")[[1]][5])
  Kx_2 <- as.double(strsplit(output_2[7,], " ")[[1]][5])
  Kx_3 <- as.double(strsplit(output_3[7,], " ")[[1]][5])
  Kx_4 <- as.double(strsplit(output_4[7,], " ")[[1]][5])
  Kx_5 <- as.double(strsplit(output_5[7,], " ")[[1]][5])
  Kx_6 <- as.double(strsplit(output_6[7,], " ")[[1]][5])
  Kx_7 <- as.double(strsplit(output_7[7,], " ")[[1]][5])
  #===============================================================================
  kr_0 <- as.double(strsplit(output_0[8,], " ")[[1]][4])
  kr_1 <- as.double(strsplit(output_1[8,], " ")[[1]][4])
  kr_2 <- as.double(strsplit(output_2[8,], " ")[[1]][4])
  kr_3 <- as.double(strsplit(output_3[8,], " ")[[1]][4])
  kr_4 <- as.double(strsplit(output_4[8,], " ")[[1]][4])
  kr_5 <- as.double(strsplit(output_5[8,], " ")[[1]][4])
  kr_6 <- as.double(strsplit(output_6[8,], " ")[[1]][4])
  kr_7 <- as.double(strsplit(output_7[8,], " ")[[1]][4])
  #===============================================================================
  peri <- as.double(strsplit(output_0[6,], " ")[[1]][3])
  #===============================================================================
  Conductivities <- data.frame(Barrier = c("b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7"),
                               kr = c(kr_0, kr_1, kr_2, kr_3, kr_4, kr_5, kr_6, kr_7),
                               Kx = c(Kx_0, Kx_1, Kx_2, Kx_3, Kx_4, Kx_5, Kx_6, Kx_7),
                               perimeter = c(peri))
  
  return(Conductivities)
}

#===============================================================================
#===============================================================================

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

set_conductivities <- function(Conductivities, 
                               conv_kr   = 0.001157407,
                               conv_kx   = 1.157407e-09,
                               threshold = 3, 
                               Barriers  = NULL){

  #' Process and transform conductivities extracted from GRANAR-MECHA pipeline.
    
  if(is.null(Barriers)){
    stop("Please set Barriers.")
  }
  
  colnames(Conductivities)[1:2] <- c("root", "x")
  # Virtual_Roots <- merge(x = Virtual_Roots, y = Parameters[,c(1,2,3)])
  
  #==========================================================================
  # Convert Kr from cm hPa-1 d to m s-1 Mpa-1
  # cm HPa-1 d-1 = 0.01m * (100 * 10-6 MPa)-1 * (24*60*60 s)-1
  # m MPa-1 s-1  = 0.01 cm * (1e-4)^-1 * (24*60*60)^-1
  # conv_kr <- 0.01 * (100 * 1e-06)^-1 * (24*60*60)^-1
  Conductivities$kr <- Conductivities$Kr * conv_kr
  
  Conductivities$radius <- 10*Conductivities$perimeter/(2*pi) # cm -> mm
  Conductivities$Kr <- Conductivities$kr * Conductivities$perimeter * 1e-3 # because radius is in mm and kr in m...
  
  # Convert Kx from cm4 hPa-1 d-1 to m4 MPa-1 s-1
  # conv_kx <- (0.01^4) * (100 *1e-6)^-1 * (24*60*60)^-1
  Conductivities$Kx <- Conductivities$Kx * conv_kx
  
  # Convert kAQP from cm hPa-1 d-1 to m MPa-1 s-1 (same as kr)
  Conductivities$kAQP <- Conductivities$kAQP * conv_kr
  Conductivities$kAQP <- round(x = Conductivities$kAQP, digits = 10)
  
  #==========================================================================
  
  # Apply remove outliers : change threshold
  Conductivities <- remove_outliers(Conductivities,
                                    threshold =  threshold)
  
  # SELECT GIVEN BARRIERS
  Conductivities_processed <- Conductivities %>% filter(Barrier %in% Barriers)
  
  return(Conductivities_processed)
  
}

#===============================================================================
#===============================================================================

radius.lm <- function(x,
                   timing     = c(7, 25),
                   radius.min = 0.1887, 
                   radius.max = 1,
                   a1         = -7.94, 
                   a2          = 0.245
                   ){
  
  #' Return radius as linear function of age, timing and linear parameters
  #' @param x The age of the segment, or a vector of ages
  #' @param timing A vector containing 2 values of timing : first value is end of T1, second value is the begining of secondary growth
  #' @param b1 Default value of radius.
  #' @param b2 Parametric value of the exponential prediction.
  #' @param a Parametric value of the exponential prediction.
  
  radius.vec <- c()
  for(xi in x){
    if(xi < timing[2]){
      temp_radius = radius.min
    }else{
      temp_radius = exp(a1 + a2*(xi + 25-timing[2]))
      
      # Security Checks
      if(temp_radius < radius.min){
        temp_radius <- radius.min
      }
      if(temp_radius > radius.max){
        temp_radius <- radius.max
      }
    }
    radius.vec <- c(radius.vec, temp_radius)
  }
  
  return(radius.vec)
}

#===============================================================================
#===============================================================================

Kx.lm <- function(x, 
                  radius, 
                  coefficients,
                  Kx.min = 1.8e-11,
                  Kx.max = 1.0e-4
                  ){
  
  Kx <- exp(coefficients$a + coefficients$x*x + coefficients$r*radius)
  
  
  # Security Checks
  if(Kx > Kx.max){
    Kx <- Kx.max
  }
  if(Kx < Kx.min){
    Kx <- Kx.min
  }
  
  return(Kx)
}

#===============================================================================
#===============================================================================

kr.lm <- function(x, 
                  MS,
                  radius,
                  timing = c(7, 25),
                  kAQP   = 1e-04,
                  coefficients,
                  kr.min = 1e-16,
                  kr.max = 1e-3
                  ){
  
  #' Compute radial conductivity kr as a linear function of radius, kAQP and Maturation Stages
  
  if(MS == "M1"){
    kr <- coefficients$a + coefficients$x*x + coefficients$r*radius + (coefficients$kAQP)*kAQP 
  }
  
  else if(MS == "M2"){
    kr <- coefficients$a + coefficients$x*x + coefficients$r*radius + (coefficients$kAQP + coefficients$M2_kAQP)*kAQP + coefficients$M2
  }
  
  else if(MS == "M3"){
    kr <- coefficients$a + coefficients$x*x + coefficients$r*radius + (coefficients$kAQP + coefficients$M3_kAQP)*kAQP + coefficients$M3
  }
  
  # First maturation stage : barier = b5
  else if(MS == "T1"){
    kr <- coefficients$a + coefficients$x*x + coefficients$r*radius + (coefficients$kAQP + coefficients$T1_kAQP)*kAQP + coefficients$T1
  }
  # Second maturation stage : barrier = b6
  else if (MS == "T2"){
    kr <- coefficients$a + coefficients$x*x + coefficients$r*radius + (coefficients$kAQP + coefficients$T2_kAQP)*kAQP + coefficients$T2
  }
  # Third maturation stage : barrier  = b4
  else if (MS == "T3"){
    kr <- coefficients$a + coefficients$x*x + coefficients$r*radius + (coefficients$kAQP + coefficients$T3_kAQP)*kAQP + coefficients$T3
  }
  else{print("error in x")}
  
  # print(kr)
  
  # Security Checks
  if(kr > kr.max){
    kr <- kr.max
  }
  if(kr < kr.min){
    kr <- kr.min
  }
  
  return(kr)
}

#===============================================================================
#===============================================================================
# 
# radial_conductance_reg <- function(x, MS, kAQP, radius){
#     
#     # First maturation stage : barier = b5
#     if(MS == "T1"){
#       Kr <- 1.722e-09 + 1.481e-10*x - 1.478e-08*radius + 9.561e-06*kAQP
#     }
#     # Second maturation stage : barrier = b6
#     else if (MS == "T2"){
#       Kr <- 1.722e-09 + 1.481e-10*x - 1.478e-08*radius + (9.561e-06 -3.848e-06)*kAQP -4.269e-10
#     }
#     # Third maturation stage : barrier  = b4
#     else if (MS == "T3"){
#       Kr <- 1.722e-09 + 1.481e-10*x - 1.478e-08*radius + (9.561e-06 -7.508e-06)*kAQP -1.595e-09 
#     }
#     else{print("error in x")}
#     
#     return(Kr)
#     
# }

#===============================================================================
#===============================================================================

run_mecha <- function(output_path = "MECHA/Projects/granar/out/Tomato/Root/Project_Test/results/"){
  
  # use_condaenv("GRANAR-MECHA") # choose MECHA environment
  
  # Moving file to MECHA
  # file.copy(paste0(anatomy_path, anatomy_file), "MECHA/cellsetdata/")
  # file.rename(paste0("MECHA/cellsetdata/", anatomy_file), "MECHA/cellsetdata/current_root.xml")
  
  temp_name2 <- str_split(anatomy_file, pattern = ".xml")[[1]][1]
  
  # initiate try
  tryCatch(
    expr= {
      print("Launching MECHA...")
      try(py_run_file("MECHA/MECHAv4_TRoot.py"))
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
      kr_0 <- as.double(strsplit(output_0[8,], " ")[[1]][4])
      kr_1 <- as.double(strsplit(output_1[8,], " ")[[1]][4])
      kr_2 <- as.double(strsplit(output_2[8,], " ")[[1]][4])
      kr_3 <- as.double(strsplit(output_3[8,], " ")[[1]][4])
      kr_4 <- as.double(strsplit(output_4[8,], " ")[[1]][4])
      kr_5 <- as.double(strsplit(output_5[8,], " ")[[1]][4])
      kr_6 <- as.double(strsplit(output_6[8,], " ")[[1]][4])
      kr_7 <- as.double(strsplit(output_7[8,], " ")[[1]][4])
      kr_8 <- as.double(strsplit(output_8[8,], " ")[[1]][4])
      kr_9 <- as.double(strsplit(output_9[8,], " ")[[1]][4])
      #===============================================================================
      peri <- as.double(strsplit(output_0[6,], " ")[[1]][3])
      print("     Success of MECHA execution. Saving data...")
      
      # Save conductivities
      Conds <- data.frame(Name = temp_name2,
                           Barrier = c("b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9"),
                           kr = c(kr_0, kr_1, kr_2, kr_3, kr_4, kr_5, kr_6, kr_7, kr_8, kr_9),
                           Kx = c(Kx_0, Kx_1, Kx_2, Kx_3, Kx_4, Kx_5, Kx_6, Kx_7, Kx_8, Kx_9),
                           perimeter = peri)
      
      return(Conds)
      # write.csv(file = paste0("Results/Quantified_Anatomies/", temp_name2, ".csv"), x = Temp_K, row.names = F)
      
    }
  )
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
