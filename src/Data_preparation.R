# This script load the xlsx quantification files, create
# - Plants : table with plant_ids, length and growth rate
# - Segments : table for each plant with the segments taken, the segments length and ages
# - anatomy1 : full data per anatomy
# - anatomy2 : processed data as input for GRANAR per anatomy
# - CrossSections : table with CS_id and all id info per CS

data_path  <- "./Data/Anatomy/"
q_path     <- "./Data/Quantifications/" # Path to quantifications
param_path <- "./src/Params/" 

# ==============================================================================
# LOAD TABLES
# ==============================================================================
print("Loading tables...")

Plants     <- read_xlsx(paste0(data_path,"Plants.xlsx"), sheet = "Plants")
Segments   <- na.omit(read_xlsx(paste0(data_path,"Plants.xlsx"), sheet = "Segments"))
Parameters <- read.csv(paste0(data_path, "Parameters.csv"))

print("Creating anatomy1...")
anatomy1   <- Merge_Quantifications("Data/Quantifications/") # Process every quantification
# anatomy1$coloration <- as.character(anatomy1$coloration)

# ==============================================================================
# SEGMENTS
# ==============================================================================
print("Processing segments length and ages...")

# Length and Age
Segments$length   <- Segments$end - Segments$begin
Segments$position <- 1
Segments$age      <- 1

for (i in seq(1, length(Segments$plant_id))) {
  Segments[i,]$position <- round(mean(c(Segments[i,]$end, Segments[i,]$begin)), digits = 2)
  temp_GR <- (Plants$growth_rate[Plants$plant_id == Segments[i,]$plant_id])
  Segments[i,]$age <- round(Segments[i,]$position/temp_GR , digits = 2)
}

# ==============================================================================
# PROCESS INTO GRANAR INPUT
# ==============================================================================
print("Processing anatomy2...")

list <- list.files(path = q_path, pattern = "xlsx") # Make a list of all files names
anatomy2 <- data.frame() # Initiate the loop

for(i in seq(1:length(list))){
  
  print(list[i])
  temp_df  <- extract_ids(list, i) # Generate the row of ids
  path     <- paste0(q_path, list[i])  # Extract data
  data_all <- extract_all(path)

  # Correct secondary growth
  if(data_all$value[data_all$name == "phloem" & data_all$type == "proportion"] == 0){
    data_all$value[data_all$name == "secondarygrowth"] <-  0
    data_all$value[data_all$name == "phloem" & data_all$type == "n_layers"] <-  0
  }

  data_all  <- left_join(data_all, Parameters[c("name", "type", "param_id")], by = c("name", "type"))
  # print(data_all2$value[data_all2$name == "phloem" & data_all2$type == "n_layers"])

  data_all  <- data_all[c("param_id", "value")]
  data_all  <- merge(data_all, temp_df) # Merge ids and data
  anatomy2  <- rbind(anatomy2, data_all)
}

# ==============================================================================
# CREATE CROSS SECTION TABLE
# ==============================================================================
print("Creating CrossSections table and XML files...")

# Merge anatomy2 with param names and types
anatomy2a <- left_join(anatomy2, Parameters[c("param_id", "name", "type")], by = "param_id")

CrossSections <- data.frame()

for(i in unique(anatomy2a$id)){

  # CrossSection table
  temp <- anatomy2a %>% filter(id == i)
  temp2 <- temp[1,]

  temp_id <- paste0(temp2$plant_id,
                    "_",
                    temp2$segment,
                    "_",
                    temp2$coloration,
                    "_",
                    temp2$cut,
                    "_",
                    temp2$zoom,
                    "_",
                    temp2$repetition)

  age_temp <- Segments$age[Segments$plant_id == temp2$plant_id & Segments$segment == temp2$segment]
  tempDF <- data.frame(CS_id = c(temp_id),
                       plant_id = c(temp2$plant_id),
                       segment = c(temp2$segment),
                       coloration = c(temp2$coloration),
                       cut = c(temp2$cut),
                       zoom = c(temp2$zoom),
                       repetition = c(temp2$repetition),
                       age = c(age_temp))
  CrossSections <- rbind(CrossSections, tempDF)

  # Save XML
  # temp_id
  # param_xml <- subset(temp, select = c(name, type, value))
  # t_path <- paste0("Params/", temp_id, ".xml")
  # write_param_xml2(param_xml, t_path)
}

# ==============================================================================
# CHANGE ANATOMY1 AND ANTOMY2 IDs
# ==============================================================================

# Anatomy1
anatomy1 <- left_join(x = anatomy1, y = CrossSections)
anatomy1 <- anatomy1[c("CS_id", "name", "type", "value")]
anatomy1$id <- seq.int(nrow(anatomy1))

# Anatomy2
anatomy2 <- left_join(x = anatomy2, y = CrossSections)
anatomy2 <- anatomy2[c("CS_id", "param_id", "value")]
# anatomy2$id <- seq.int(nrow(anatomy2))

# Change phloem n_files to zero when primary growth
for(csid in unique(anatomy2$CS_id)){
  # print(csid)
  if(anatomy2$value[anatomy2$CS_id == csid & anatomy2$param_id == "phloem_proportion"] == 0){
    anatomy2$value[anatomy2$CS_id == csid & anatomy2$param_id == "phloem_n_files"] = 0
  }
}

# ==============================================================================
# GENERATE XML FILES
# ==============================================================================
print("Creating XML files...")

# Merge anatomy2 with param names and types
anatomy2a <- left_join(x = anatomy2, y = Parameters[,c(1,2,3)], by = "param_id")

for(i in unique(anatomy2a$CS_id)){
  # name <- unique(anatomy2a$CS_id)[i]
  print(paste0("exporting ",i))
  param_test <- anatomy2a %>% filter(CS_id == i)
  param_test <- subset(param_test, select = c(name, type, value))
  t_path     <- paste0(param_path, i, ".xml")
  write_param_xml2(param_test, t_path)
}

# ==============================================================================

# print("Making Parameters table")
# Parameters <- (anatomy2 %>% filter(CS_id == anatomy2$CS_id[1]))[c(2,3)]
# # Parameters$param_id <- 1
# for (i in seq(1,length(Parameters$name))) {
#   Parameters$param_id[i] <- paste0(Parameters$name[i],"_",Parameters$type[i])
# }
#
# Parameters$units <- c("-", "-", "-", "-",
#            "-", "-", "?m^2", "?m^2",
#            "-", "mm", "?m^2", "?m^2",
#            "?m^2", "?m^2", "-", "mm",
#            "-","mm", "-", "-",
#            "mm","-", "-", "mm",
#            "-", "-", "mm", "-",
#            "-", "maturation_stage", "maturation_stage", "-",
#            "mm", "-", "-", "-")

# ==============================================================================


print("Exporting data...")
write.csv(Plants, "Data/Plants.csv", row.names = F)
write.csv(anatomy1, "Data/anatomy1.csv", row.names = F)
write.csv(Segments, "Data/Segments.csv", row.names = F)
write.csv(anatomy2, "Data/anatomy2.csv", row.names = F)
write.csv(CrossSections,"Data/CrossSections.csv", row.names = F)
# write.csv(Parameters, "Data/Parameters.csv", row.names = F)
# ==============================================================================
# ==============================================================================
print("Data preparation successfull")

