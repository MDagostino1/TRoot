# Create folder for the Scenario
if(dir.exists(path.out)){
  unlink(paste0(path.out, "*"), recursive = F)
}else{
  dir.create(path.out)
}

# ==============================================================================
# LOAD CONDUCTIVITIES SCENARIOS
# ==============================================================================

# Convert m[4]*s-1*MPa-1 back to cm[4]*d-1*HPa-1
df            <- Conds.tot %>% 
  filter(Scenario_ID == Scenario_ID_i) # TO CHANGE ; HERE WE SELECT ONLY THE SCENARIO IN INPUT
df$kr         <- df$kr/conv_kr
df$Kx         <- df$Kx/conv_kx

# Define root orders
orders_df     <- tibble(order_id = c(1,2,3), 
                        order    = c("Taproot", "Lateral", "LongLateral")
                        )

# Create Conds.conv
Conds.conv           <- df[c("x", "kr", "Kx")] 
colnames(Conds.conv) <- c("x", "kr", "kx")
Conds.conv           <- Conds.conv %>% gather(kr, kx, key = "type", value = "y")
Conds.conv           <- merge(Conds.conv, orders_df)
Conds.conv$id        <- seq(1, nrow(Conds.conv))
Conds.conv           <- Conds.conv[c("order_id", "order", "x", "type", "y", "id")]

# Add a zero x
Conds.conv           <- rbind(Conds.conv, Conds.conv %>% filter(x == 1) %>% mutate(x = 0))

data2plot <- Conds.conv
data2plot$y[data2plot$type == "kr"] <- data2plot$y[data2plot$type == "kr"]/max(data2plot$y[data2plot$type == "kr"])
data2plot$y[data2plot$type == "kx"] <- data2plot$y[data2plot$type == "kx"]/max(data2plot$y[data2plot$type == "kx"])

cond_plot <- ggplot(data2plot) +
  geom_point(aes(x = x, y = y, color = type)) +
  geom_line(aes(x = x, y = y, color = type)) +
  theme_classic()

cond_plot

ggsave(filename = "Conds_plot.svg", plot = cond_plot, device = "svg", path = path.out, 
       width = 4, height = 3)

# ==============================================================================
# LOOP MARSHAL
# ==============================================================================

conds <- Conds.conv
# MARSHAL PARAMETERS
# conds        <- read.csv("CRB-MARSHAL/inputs/conductivities3.csv")
soil           <- read.csv(paste0(MPath, "inputs/soil.csv"))

# ============================================================================ #
results <- run.MARSHAL.loop(RSA_list, 
                            RSA.all, 
                            Sim_ID = Sim_ID, 
                            conds, 
                            soil, 
                            tmin = tmin, tmax = tmax, 
                            step = it_length,       # Decomposition of RSA timing 
                            timing1 = 7, timing2 = 20)           # SG timing
# ============================================================================ #

RSHA.all  <- results$RSHA.all
Macro.all <- results$Macro.all

write.csv(RSHA.all, paste0(path.out, "RSHA_all.csv"), row.names = F)
write.csv(Macro.all, paste0(path.out, "Macro_all.csv"), row.names = F)

# Check SUF
# ggplot(RSHA.all %>% filter(RSHA_id == RSHA.all$RSHA_id[-1])) +
#   geom_point(aes(x = time, y = SUF, color = as.factor(branchID))) 

# ==============================================================================
# MAKE PLOTS
# ==============================================================================
# Load results by MARSHAL
# RSHA.all  <- read.csv(paste0(path.out, "RSHA_all.csv"))
# Macro.all <- read.csv(paste0(path.out, "Macro_all.csv"))
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
  plot.CRBM(RSHA.all, Macro.all, ti, path.plot = plot.path, Krs.max = max(Macro.all$Krs))
}

# Generate GIF 
library(magick)
img          <- list.files(plot.path, pattern = ".png")
img_list     <- lapply(paste0(plot.path, img), image_read)
img_join     <- image_join(img_list)
img_animated <- image_animate(img_join, delay = 10)
image_write(image = img_animated,
            path = paste0(path.out, "animated.gif"))

# ==============================================================================
# ANALYSE SUF
# ==============================================================================

RSHA.all$type <- as.factor(RSHA.all$type)

data2plot <- RSHA.all %>% filter(age == tmax) %>% 
  mutate(zr = round(z1)) %>% 
  group_by(RSHA_id, zr, type) %>% 
  summarise(SUF_sum = sum(SUF)) 

# ggplot(data2plot) +
#   geom_bar(aes(x = -zr, y = SUF_sum, fill = type), 
#            stat = "identity", position = position_dodge(width = 0.5),
#            alpha = 1) +
#   facet_wrap(~RSHA_id)

# ==================================================

data2plot2 <- data2plot %>% 
  group_by(zr, type) %>% 
  summarise(SUF_sum_mean = mean(SUF_sum),
            SUF_sum_sd   = sd(SUF_sum))

SUF_plot <- ggplot(data2plot2) +
  
  geom_bar(aes(x = -zr, 
               y = SUF_sum_mean,
               fill = type), stat = "identity", 
           alpha = 1
  ) +
  
  # geom_point(aes(x = -zr, y = SUF_sum_mean),color = "grey") +
  
  geom_errorbar(aes(x = -zr, 
                    ymax = SUF_sum_mean+SUF_sum_sd, 
                    ymin = SUF_sum_mean-SUF_sum_sd,
  ),
  color = "grey"
  ) +
  
  facet_wrap(~type, ncol = 1) +
  xlab("Soil depth [cm]") +
  ylab("Standart Uptake Fraction [-]") +
  theme_bw()

ggsave(filename = "SUF_plot.svg", plot = SUF_plot, device = "svg", path = path.out, 
       width = 4, height = 4)

