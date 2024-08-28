# update : 21/11/2022 ; update extract parenchyma (diameter = 2x radius), code transform_param
# Add :    07/01/2023 ; add anatomy_plot()

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

nice_boxplot1 <- function(data, X, Y, Z){
  # @data = a dataframe
  # @X = variable x        (ex : data$plant_id)
  # @Y = variable y        (ex : data$length)
  # @Z = variable to group (ex : data$genotype)
  
  g <- data %>% ggplot(aes(x = X, y = Y)) +
    geom_point() +
    geom_boxplot() +
    
    #facet_wrap(~ Z) +
    
    theme_bw()   
  return(g)
  
}


#===============================================================================
#===============================================================================

nice_boxplot2 <- function(data = data, X = x, Y = y, Z = z){
  
  
  g <- ggplot(data, aes(x=X, y=Y, fill=Z)) +
    
    geom_boxplot() +
    
    scale_fill_viridis(discrete = TRUE, alpha=0.6) + # Colors
    geom_jitter(color="black", size=0.4, alpha=0.9) + # Add points
    
    # Theme params
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    
    theme_bw() +
    theme(axis.line = element_line(color='black'),
          
          # Remove grid
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          
          # Change font
          text = element_text(family = "A"),
          
          # Remove x ticks
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          
          # Remove legend title
          #legend.title=element_blank()
          
          # Remove legend
          legend.position="none"
          
          # Remove panel borders 
          #panel.border = element_blank()
          
          
    )
  
  return(g)}



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
  Kr_0 <- as.double(strsplit(output_0[8,], " ")[[1]][4])
  Kr_1 <- as.double(strsplit(output_1[8,], " ")[[1]][4])
  Kr_5 <- as.double(strsplit(output_5[8,], " ")[[1]][4])
  Kr_6 <- as.double(strsplit(output_6[8,], " ")[[1]][4])
  #===============================================================================
  peri <- as.double(strsplit(output_0[6,], " ")[[1]][3])
  #===============================================================================
  Conductivities <- data.frame(Barrier = c("b0", "b1", "b5", "b6"),
                               Kr = c(Kr_0, Kr_1, Kr_5, Kr_6),
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
  Kr_0 <- as.double(strsplit(output_0[8,], " ")[[1]][4])
  Kr_1 <- as.double(strsplit(output_1[8,], " ")[[1]][4])
  Kr_2 <- as.double(strsplit(output_2[8,], " ")[[1]][4])
  Kr_3 <- as.double(strsplit(output_3[8,], " ")[[1]][4])
  Kr_4 <- as.double(strsplit(output_4[8,], " ")[[1]][4])
  Kr_5 <- as.double(strsplit(output_5[8,], " ")[[1]][4])
  Kr_6 <- as.double(strsplit(output_6[8,], " ")[[1]][4])
  Kr_7 <- as.double(strsplit(output_7[8,], " ")[[1]][4])
  #===============================================================================
  peri <- as.double(strsplit(output_0[6,], " ")[[1]][3])
  #===============================================================================
  Conductivities <- data.frame(Barrier = c("b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7"),
                               Kr = c(Kr_0, Kr_1, Kr_2, Kr_3, Kr_4, Kr_5, Kr_6, Kr_7),
                               Kx = c(Kx_0, Kx_1, Kx_2, Kx_3, Kx_4, Kx_5, Kx_6, Kx_7),
                               perimeter = c(peri))
  
  return(Conductivities)
}

#===============================================================================
#===============================================================================

remove_outliers <- function(data, threshold = 2) {
  
  new_data <- data.frame()
  
  for (temp_x in unique(data$x)){
    temp_data = data[(data$x == temp_x),]
    
    # Calculate Z-scores for each numeric column
    z_scores <- apply(temp_data[c(4,5,6,7)], 2, function(x) (x - mean(x)) / sd(x))
    
    # Identify rows with any Z-score above the threshold
    outlier_rows <- rowSums(abs(z_scores) > threshold) > 0
    
    # Remove outlier rows from the dataframe
    temp_data <- temp_data[!outlier_rows, ]
    new_data <- rbind(new_data, temp_data)
  }
  
  return(new_data)
}

#===============================================================================
#===============================================================================

set_conductivities <- function(Conductivities, threshold = 2){

  colnames(Conductivities)[c(1,2)] <- c("root", "x")
  # Virtual_Roots <- merge(x = Virtual_Roots, y = Parameters[,c(1,2,3)])
  
  #==========================================================================
  # Convert Kr from cm hPa-1 d to m s-1 Mpa-1
  # cm HPa-1 d-1 = 0.01m * (100 * 10-6 MPa)-1 * (24*60*60 s)-1
  conv <- 0.01 * (100 * 1e-06)^-1 * (24*60*60)^-1
  Conductivities$Kr2 <- Conductivities$Kr * conv
  
  #==========================================================================
  
  # Apply remove outliers : change threshold
  Conductivities <- remove_outliers(Conductivities, 
                                    threshold =  threshold)

  # ALL CONDUCTIVITIES
  # Conductivities <- Conductivities %>% filter(Barrier == "b1" | Barrier == "b2" | Barrier == "b3" | Barrier == "b4" | Barrier == "b5" | Barrier == "b6" | Barrier == "b7")
  # Conductivities2 <- Conductivities
  # Conductivities2$x <- as.factor(Conductivities2$x)
  
  # ONLY TOMATO BARRIERS
  Conductivities_dico <- Conductivities %>% filter(Barrier == "b5" | Barrier == "b7" | Barrier == "b4")
  # Conductivities_dico2 <- Conductivities_dico
  # Conductivities_dico2$x <- as.factor(Conductivities_dico2$x)
  
  return(Conductivities_dico)
  
}

#===============================================================================
#===============================================================================

set_conductivities2 <- function(Conductivities, threshold = 2){
  
  colnames(Conductivities)[c(1,2)] <- c("root", "x")
  # Virtual_Roots <- merge(x = Virtual_Roots, y = Parameters[,c(1,2,3)])
  
  #==========================================================================
  # Convert Kr from cm hPa-1 d to m s-1 Mpa-1
  # cm HPa-1 d-1 = 0.01m * (100 * 10-6 MPa)-1 * (24*60*60 s)-1
  conv <- 0.01 * (100 * 1e-06)^-1 * (24*60*60)^-1
  Conductivities$Kr2 <- Conductivities$Kr * conv
  
  #==========================================================================
  
  # Apply remove outliers : change threshold
  Conductivities <- remove_outliers(Conductivities, 
                                    threshold =  threshold)
  
  # ALL CONDUCTIVITIES
  # Conductivities <- Conductivities %>% filter(Barrier == "b1" | Barrier == "b2" | Barrier == "b3" | Barrier == "b4" | Barrier == "b5" | Barrier == "b6" | Barrier == "b7")
  # Conductivities2 <- Conductivities
  # Conductivities2$x <- as.factor(Conductivities2$x)
  
  # ONLY TOMATO BARRIERS
  Conductivities_mono <- Conductivities %>% filter(Barrier == "b1" | Barrier == "b3" | Barrier == "b6" | Barrier == "b4")
  # Conductivities_dico2 <- Conductivities_dico
  # Conductivities_dico2$x <- as.factor(Conductivities_dico2$x)
  
  return(Conductivities_mono)
  
}

#===============================================================================
#===============================================================================

# Function that return a radius from a age input. Age = value or vector, b1 is the mean of the first part of the curve, b2 and a are parameters of the regression of the second part. 
radius_reg <- function(age, b1 = 0.18, b2 = -7.5, a = 0.24){
  
  radiusDF <- data.frame()
  
  for(age_i in age){
    # print(age_i)
    if(age_i < 25){
      temp_radius = b1
    }else
    {temp_radius = exp(b2+a*age_i)}
    
    # print(temp_radius)
    radiusDF <- rbind(radiusDF, data.frame(age = age_i, radius = temp_radius))
  }
  
  return(radiusDF)
}

#===============================================================================
#===============================================================================

# Prerequire radius_reg()
radial_reg <- function(x, timing = c(7, 25)){
  
  radius <- radius_reg(x)$radius
  
  # First maturation stage : barier = b5
  if(x <= timing[1]){
    c_i <- 5.9e-08 -2.36e-08*radius + 6.44e-08 -2.079e-07*radius
  }
  # Second maturation stage : barrier = b6
  else if (x > timing[1] & x <= timing[2]){
    c_i <- 5.9e-08 -2.36e-08*radius + 3.86e-08 -1.28e-07*radius
  }
  # Third maturation stage : barrier  = b4
  else if (x > timing[2]){
    c_i <- 5.9e-08 -2.36e-08*radius
  }
  else{print("error in x")}
  
  return(c_i)
  
}

#===============================================================================
#===============================================================================

axial_reg <- function(x){
  if(x < 25){
    kx <- exp(-4.75 + 0.105*x)
  }else{
    kx <- exp(-12.5 + 0.437*x)
  }
  
  return(kx)
}

#===============================================================================
#===============================================================================

# Prerequire radius_reg()
radial_reg2 <- function(x, timing = c(7, 25)){
  
  radius <- 0.18
    
  # First maturation stage : barier = b5
  if(x <= timing[1]){
    c_i <- 5.9e-08 -2.36e-08*radius + 6.44e-08 -2.079e-07*radius
  }
  # Second maturation stage : barrier = b6
  else if (x > timing[1]){
    c_i <- 5.9e-08 -2.36e-08*radius + 3.86e-08 -1.28e-07*radius
  }
  else{print("error in x")}
  
  return(c_i)
  
}

#===============================================================================
#===============================================================================

axial_reg2 <- function(x){
  # if(x < 25){
    kx <- exp(-4.75 + 0.105*x)
  # }else{
  #   kx <- exp(-12.5 + 0.437*x)
  # }
  
  return(kx)
}

#===============================================================================
#===============================================================================

