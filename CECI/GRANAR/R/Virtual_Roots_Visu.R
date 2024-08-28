vroot_plot <- function(Data = data,
                         Title = title){
  
  ggplot(data = Data, 
         aes(x = x, 
             y = y
             )) +
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
          panel.grid.minor = element_blank(),
          # Change font
          # text = element_text(family = "A"),
          # Remove x ticks
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          # Remove legend title
          #legend.title=element_blank()
          # Remove legend
          legend.position="none"
          # Remove panel borders 
          #panel.border = element_blank()
    ) +
    ggtitle(Title)
}

#===============================================================================

Virtual_Roots_Visu <- function(Virtual_Roots){
  
  combis <- unique(Virtual_Roots[,c("name", "type")])
  
  # loop through the params (1 param = 1 combi of name and type)
  for (i in seq(1:length(combis$name))) {
    
    # print(combis[i,])
    temp_name <- combis[i,][[1]]
    temp_type <- combis[i,][[2]]
    
    # create a subset of all roots within the param
    root_subset <- Virtual_Roots %>% filter(name == temp_name, type == temp_type)
    
    # generate the plot
    ptemp <- vroot_plot(root_subset, paste0(temp_name, " | ", temp_type))
    assign(paste0("plot",i), ptemp)
  }
  
  # Merge plots
  p <- grid.arrange(plot2, plot3, plot4, plot5, plot6, plot8,plot11, plot13, plot14, plot15, plot16, plot17, plot18,
                    ncol = 4)
  
  return(p)
}

#===============================================================================

vroot_plot2 <- function(Data = data,
                       Title = title){
  
  ggplot(data = Data, 
         aes(x = x, 
             y = y,
             color = root)) +                                                   # Change here to add/supp colored root
    geom_point() +
    # geom_line() +                                                               # Change here to add/supp line between same node of one root
    
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
          panel.grid.minor = element_blank(),
          # Change font
          # text = element_text(family = "A"),
          # Remove x ticks
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          # Remove legend title
          #legend.title=element_blank()
          # Remove legend
          legend.position="none"
          # Remove panel borders 
          #panel.border = element_blank()
    ) +
    ggtitle(Title)
}

Virtual_Roots_Visu2 <- function(Virtual_Roots){
  
  combis <- unique(Virtual_Roots[,c("name", "type")])
  
  # loop through the params (1 param = 1 combi of name and type)
  for (i in seq(1:length(combis$name))) {
    
    # print(combis[i,])
    temp_name <- combis[i,][[1]]
    temp_type <- combis[i,][[2]]
    
    # print(c(temp_name, temp_type))
    
    # create a subset of all roots within the param
    root_subset <- Virtual_Roots %>% filter(name == temp_name, type == temp_type)
    
    # generate the plot
    ptemp <- vroot_plot2(root_subset, paste0(temp_name, " | ", temp_type))
    assign(paste0("plot",i), ptemp)
    
    # print(paste0("plot", i))
  }
  
  # Merge plots
  p <- grid.arrange(plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot12, plot14, plot16, plot17, plot18, plot19, plot20, plot21,
                    ncol = 4)
  
  return(p)
}