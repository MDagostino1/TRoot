# Create folder for the Scenario
if(dir.exists(path.out)){
  unlink(paste0(path.out, "*"), recursive = F)
}else{
  dir.create(path.out)
}

# ==============================================================================
# LOAD CONDUCTIVITIES SCENARIOS
# ==============================================================================

cond_plot <- ggplot(Conds, aes(x = x, y = y, color = type, group = order_id)) +
  geom_point() +
  geom_line() +
  facet_wrap(~type, ncol=1, scales = "free_y") +
  theme_few()

cond_plot
ggsave(filename = paste0(S_ID, "_Conds.svg"), 
       plot = cond_plot, device = "svg", 
       path = path.out, 
       width = 4, height = 4)

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

# ==============================================================================
# ANALYSE SUF
# ==============================================================================

RSHA.all$type <- as.factor(RSHA.all$type)

data2plot <- RSHA.all %>% filter(age == tmax) %>% 
  mutate(zr = round(z1))%>% 
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

ggsave(filename = paste0(S_ID, "_SUF.svg"), plot = SUF_plot, device = "svg", path = path.out, 
       width = 4, height = 4)

