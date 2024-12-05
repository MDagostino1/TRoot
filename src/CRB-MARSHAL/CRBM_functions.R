reconnect.nodes <- function(all_roots){
  # Reconnect nodes of a RSA
  
  all_roots <- all_roots%>%arrange(time)
  all_roots <- all_roots%>%dplyr::mutate(length = sqrt((x1-x2)^2+(y1-y2)^2+(z1-z2)^2))
  all_roots$node2ID <- 1:nrow(all_roots)
  all_roots$node1ID[all_roots$branchID == 1][1] <- 0
  all_roots$node1ID[all_roots$branchID == 1][-1] <- which(all_roots$branchID == 1)[-length(which(all_roots$branchID == 1))] # tap root ordination
  
  for(i in unique(all_roots$branchID)[-1]){
    all_roots$node1ID[all_roots$branchID == i][-1] <- which(all_roots$branchID == i)[-length(which(all_roots$branchID == 1))]
    if(all_roots$type[all_roots$branchID == i][1] %in% c(4,5)){ # connection with the collar
      all_roots$node1ID[all_roots$branchID == i][1] <- 0
    }
    if(all_roots$type[all_roots$branchID == i][1] %in% c(2,3)){ # connection with the parental root
      x1_child <- all_roots$x1[all_roots$branchID == i][1]
      y1_child <- all_roots$y1[all_roots$branchID == i][1]
      z1_child <- all_roots$z1[all_roots$branchID == i][1]
      tmp_time <- all_roots$time[all_roots$branchID == i][1]
      nearest <- all_roots%>%filter(branchID != i)%>%
        mutate(euc = sqrt((x1-x1_child)^2+ (y1 - y1_child)^2 + (z1 - z1_child)^2))
      nearest <- nearest[nearest$euc == min(nearest$euc), ]
      all_roots$node1ID[all_roots$branchID == i][1] <- nearest$node2ID[1] # oldest segments
    }
  }
  oups <- which(all_roots$node1ID == all_roots$node2ID)
  if(length(oups) > 0){
    for(o in oups){
      self_seg_age <- all_roots$time[all_roots$node2ID == o][1]
      self_seg_id <- all_roots$branchID[all_roots$node2ID == o][1]
      nearest <- all_roots%>%filter(branchID == self_seg_id, time < self_seg_age)
      all_roots$node1ID[all_roots$node2ID == o][1] <- nearest$node2ID[nearest$time == max(nearest$time)]
    }
  }
  return(all_roots)
}

# ==============================================================================

run.MARSHAL.loop <- function(RSA_list, 
                             RSA.all,
                             Scenario_id,
                             conds, 
                             soil,
                             hetero = F,
                             Psi_collar = -15000,
                             tmin = 0,
                             tmax = 30,
                             step = 1,
                             timing1 = 7,
                             timing2 = 20
                             ){
  
  # INITIATE DFs
  RSHA.all     <- tibble()
  Macro.all    <- tibble()
  
  # LOOP MARSHAL FOR TIMESTEPS
  for(ti in seq(tmin, tmax, by = step)){
    
    # Subset RSA corresponding to timestep
    RSA.all.ti <- RSA.all %>% filter(time <= ti)
    
    # UPDATE RADIUS
    RSA.all.ti$radius <- radius_reg(x_vec = max(RSA.all.ti$time) - RSA.all.ti$time, 
                                    timing = c(timing1, timing2))
    
    # Loop for each RSA
    for(RSA_id_i in unique(RSA.all.ti$RSA_id)){
      
      # Subset the corresponding RSA
      RSA_temp <- RSA.all.ti %>% filter(RSA_id == RSA_id_i)
      
      # Create RSHA_id_i
      RSHA_id_i <- paste0(RSA_id_i, "_", ti, "_", Scenario_id)
      
      cat("t : ", ti, " | RSHA_id_i : ", RSHA_id_i, "\n")
      flush.console()
      
      # RECONNECT NODES
      RSA_temp <- suppressWarnings(reconnect.nodes(RSA_temp))
      
      # ==========================================================================
      try({
        Marshal_temp <- getSUF(table_data = RSA_temp,
                               table_cond = conds,
                               table_soil = soil,
                               hetero = hetero, Psi_collar = Psi_collar);
        # Create output files        
        RSHA_temp         <- Marshal_temp$root_system
        RSHA_temp$kr      <- Marshal_temp$kr
        RSHA_temp$Kx      <- Marshal_temp$kx
        RSHA_temp$SUF     <- Marshal_temp$suf
        RSHA_temp$RSHA_id <- RSHA_id_i
        RSHA_temp$RSA_id  <- RSA_id_i
        RSHA_temp$age     <- ti
        
        Macro.temp <- tibble(tpot = Marshal_temp$tpot,
                             Krs = Marshal_temp$krs,
                             Krs_exact = Marshal_temp$krs_exact,
                             RSHA_id = RSHA_id_i,
                             RSA_id = RSA_id_i,
                             age = ti
        )
        # RBIND
        RSHA.all  <- rbind(RSHA.all, RSHA_temp)
        Macro.all <- rbind(Macro.all, Macro.temp)
      })
      # ==========================================================================
    }
  }
  
  return(list(RSHA.all= RSHA.all, 
              Macro.all = Macro.all))
}

# ==============================================================================

plot.CRBM <- function(RSHA.all, Macro.all, ti, path.plot, Krs.max){
  
  RSA_id_i   <- unique(RSHA.all$RSA_id)[1]
  
  data2plot  <- RSHA.all %>% filter(RSA_id == RSA_id_i  & age == ti)
  macro2plot_all <- Macro.all %>% filter(RSA_id == RSA_id_i)
  macro2plot_i <- Macro.all %>% filter(RSA_id == RSA_id_i & age == ti)
  
  require(cowplot)
  # PLOT ALL
  # ========================================================================== #
  p_kr <- ggplot(data2plot) + 
    geom_segment(aes(x= x1, xend = x2, y = z1, yend = z2, 
                     color = kr, size = radius)) +
    xlim(-15, 15) +
    ylim(-30, 0) +
    
    scale_size_continuous(range = c(1, 3), 
                          limits = c(min(RSHA.all$radius), max(RSHA.all$radius))) +
    scale_color_viridis_c(option = "D", 
                          limits = c(min(RSHA.all$kr), max(RSHA.all$kr))) +
    
    coord_fixed() +
    # facet_wrap(~RSA_id, nrow = 2) +
    # ggtitle(paste0("kr | time : ", ti)) +
    
    # ggtitle("Kr") +
    ggtitle(expression(k[r] ~ "[" ~ 10^{-8} ~ m ~ MPa^{-1} ~ s^{-1} ~ "]")) +
    
    theme_test()
  
  # ========================================================================== #
  p_kx <- ggplot(data2plot) +
    geom_segment(aes(x= x1, xend = x2, y = z1, yend = z2, 
                     color = Kx, size = radius)) +
    xlim(-15, 15) +
    ylim(-30, 0) +
    
    scale_size_continuous(range = c(1, 3),
                          limits = c(min(RSHA.all$radius), 
                                    max(RSHA.all$radius))) +
    scale_color_viridis_c(option = "D", 
                          limits = c(min(RSHA.all$Kx), 
                                     max(RSHA.all$Kx))) +
    coord_fixed() +
    # facet_wrap(~RSA_id, nrow = 2) +
    # ggtitle(paste0("Kx | time : ", ti)) +
    ggtitle(expression(K[x] ~ "[" ~ cm^{4} ~ hPa^{-1} ~ d^{-1} ~ "]")) +
    theme_test()
  
  # ========================================================================== #
  p_suf <- ggplot(data2plot) +
    geom_segment(aes(x= x1, xend = x2, y = z1, yend = z2, 
                     color = SUF, size = radius)) +
    xlim(-15, 15) +
    ylim(-30, 0) +
    
    # Size of radius
    scale_size_continuous(range = c(1, 3), 
                          limits = c(min(RSHA.all$radius), max(RSHA.all$radius))) +
    
    # Color of SUF
    scale_color_viridis_c(option = "magma",
                          breaks = c(min(data2plot$SUF), 
                                     max(data2plot$SUF)),
                          labels = c("low", "high"),
                          direction = 1
                          # , limits = c(0, max(RSHA.all$SUF[RSHA.all$RSA_id == RSA_id_i]))
                          ) +
    
    # Fix position of legends
    guides(
      color = guide_colorbar(order = 1),  # Color legend appears first
      size = guide_legend(order = 2)   # Size legend appears second
    ) +
    
    coord_fixed() +
    # facet_wrap(~RSA_id, nrow = 2) +
    # ggtitle(paste0("SUF | time : ", ti)) +
    
    theme_test() +
    # theme(legend.position = "none") +
    ggtitle("SUF [normalised]") 
  
  # ========================================================================== #
  p_krs <- ggplot() +
    geom_line(data = macro2plot_all, aes(x = age, y = Krs), size = 1) +
    geom_point(data = macro2plot_i, aes(x = age, y = Krs), size = 4, color = "red") +
    xlim(0, max(macro2plot_all$age)) +
    ylim(0, Krs.max) +
    ggtitle(expression(K[rs] ~ "[" ~ m^{3} ~ s^{-1} ~ MPa^{-1} ~ "]")) +
    theme_bw()
  
  # ========================================================================== #
  # MERGE ALL PLOTS
  title  <- ggdraw() + draw_label(paste0("Time : ", format(round(ti, 2), nsmall = 2)))
  p_full <- plot_grid(p_kr, p_kx, p_suf, p_krs, nrow = 2)
  p_full <- plot_grid(title, p_full, nrow = 2, rel_heights = (c(0.1, 0.7)))
  pfinal <- ggdraw(p_full) + theme(plot.background = element_rect(fill = "white"))
  ggsave(filename = paste0(path.plot, "Full", "_", sprintf("%04d", round(ti*100)), ".png"),
         plot = pfinal, device = "png", width = 10, height = 10, units = "in", dpi = 200)
}

# ==============================================================================
create.conds <- function(xmax = 50, 
                         timing1, timing2, SG = T, 
                         # kr.coeff, kx.coeff,
                         orders_id = c(1,2,3), 
                         orders_names = c("Taproot", "Lateral", "LongLateral")){
  
  conds.out <- tibble()
  for(order_id in orders_id){
    conds.temp <- tibble(order_id = order_id,
                         order = orders_names[order_id],
                         kr = 0,
                         kx = 0,
                         x = seq(0, xmax))
    conds.temp <- conds.temp %>%
      mutate(kr = sapply(x, kr_reg, timing = c(timing1, timing2)),
             kx = sapply(x, kx_reg, timing = c(timing1, timing2))
             ) %>%
      gather(kr, kx, key = type, value = y)         
    
    conds.out <- rbind(conds.out, conds.temp)
  }
  conds.out$id <- seq(1, nrow(conds.out))
  
  return(conds.out)
}
