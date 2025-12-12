# This script does :
# 1) Create a directory with path.out
# 2) Convert kr and kx back to cm HPa d
# 3) Load soil data from inputs/soil.csv
# 4) Run MARSHAL loop
# 5) Collect results of RSHA.all and Macro.all
# 6) Convert back to m MPa s
# 7) Make plots :
#     - Create a folder in plots/
#     - 


# Create folder for the Scenario
if(dir.exists(path.out)){
  unlink(paste0(path.out, "*"), recursive = F)
}else{
  dir.create(path.out)
}

# Convert back to cm HPa d because MARSHAL counts in cm
Conds$y[Conds$type == "kr"] <- Conds$y[Conds$type == "kr"] / kr.conv
Conds$y[Conds$type == "kx"] <- Conds$y[Conds$type == "kx"] / kx.conv

# ==============================================================================
# LOOP MARSHAL
# ==============================================================================

# MARSHAL PARAMETERS
soil           <- read.csv(paste0(CRBM.path, "inputs/soil.csv"))

# ============================================================================ #
results <- run.MARSHAL.loop(RSA.all    = RSA.all, 
                            Sim_ID     = S_ID, 
                            conds      = Conds, 
                            soil       = soil, 
                            tmin       = tmin, 
                            tmax       = tmax, 
                            step       = it_length,            # Decomposition of RSA timing 
                            SG_timing  = SG_timing,
                            radius.min = rmin, 
                            radius.max = rmax)   # SG timing
# ============================================================================ #

RSHA.all  <- results$RSHA.all
Macro.all <- results$Macro.all

# Convert back to m MPa s 
RSHA.all$kr   <- RSHA.all$kr * kr.conv
RSHA.all$kx   <- RSHA.all$kx * kx.conv
Macro.all$Krs <- Macro.all$Krs * krs.conv 

write.csv(RSHA.all, paste0(path.out, "RSHA_all.csv"), row.names = F)
write.csv(Macro.all, paste0(path.out, "Macro_all.csv"), row.names = F)

# Check SUF
# ggplot(RSHA.all %>% filter(RSHA_id == RSHA.all$RSHA_id[-1])) +
#   geom_point(aes(x = time, y = SUF, color = type)) 

# ==============================================================================
# MAKE PLOTS
# ==============================================================================
plot.path <- paste0(path.out, "plots/")

# Create folder for the Scenario
if(dir.exists(plot.path)){
  unlink(paste0(plot.path, "*"), recursive = F)
}else{
  dir.create(plot.path)
}

# Clean folder and save plots for each time step
cat("Saving plots... \n")
for(ti in unique(RSHA.all$age)){
  cat("ti : ", format(round(ti, 2), nsmall = 2), "\n")
  plot.CRBM(RSHA.all, 
            Macro.all, 
            ti, 
            path.plot = plot.path, 
            Krs.max = max(Macro.all$Krs),
            radius.range = radius.range
            )
}

# Generate GIF 
library(magick)
img          <- list.files(plot.path, pattern = ".png")
img_list     <- lapply(paste0(plot.path, img), image_read)
img_join     <- image_join(img_list)
img_animated <- image_animate(img_join, delay = gif.delay)
image_write(image = img_animated,
            path = paste0(path.out, S_ID, "_animation.gif"))
