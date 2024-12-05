#' Generate a big plot with the vizualization of all data per param
#'
#' @param anatomy2 Table with the measured params per cross sections
#' @param CrossSections Table with the ids of the cross sections
#' @export
#'

anatomy_visual <- function(anatomy2, CrossSections){
  # XYLEM
  anatomy2 <- merge(anatomy2, Parameters[c("param_id", "name","type")])

  xylem_stats <- anatomy2 %>% filter(name == "xylem")
  xylem_stats <- merge(x = xylem_stats, y = CrossSections[c("CS_id", "age", "plant_id")])

  xylem_n_files <- xylem_stats %>% filter(type == "n_files")
  xylem_n_cells <- xylem_stats %>% filter(type == "n_cells")
  xylem_max_size <- xylem_stats %>% filter(type == "max_size")
  xylem_cell_diameter <- xylem_stats %>% filter(type == "cell_diameter")

  X1 <- anatomy_plot(xylem_n_files, Title = "Xylem | n_files")
  X2 <- anatomy_plot(xylem_max_size, Title = "Xylem | max_size")
  X3 <- anatomy_plot(xylem_n_cells, Title = "Xylem | n_cells")
  X4 <- anatomy_plot(xylem_cell_diameter, Title = "Xylem | cell_diameter")

  # STELE
  stele_stats <- anatomy2 %>% filter(name == "stele")
  stele_stats <- merge(x = stele_stats, y = CrossSections[c("CS_id", "age", "plant_id")])

  stele_cell_diameter <- stele_stats %>% filter(type == "cell_diameter")
  stele_layer_diameter <- stele_stats %>% filter(type == "layer_diameter")
  stele_n_layers <- stele_stats %>% filter(type == "n_layers")
  stele_SD <- stele_stats %>% filter(type == "SD")

  S1 <- anatomy_plot(stele_cell_diameter, Title = "Stele | cell_diameter")
  S2 <- anatomy_plot(stele_layer_diameter, Title = "Stele | layer_diameter")
  S3 <- anatomy_plot(stele_n_layers, Title = "Stele | n_layers")
  S4 <- anatomy_plot(stele_SD, Title = "Stele | SD")

  # PHLOEM
  phloem_stats <- anatomy2 %>% filter(name == "phloem")
  phloem_stats <- merge(x = phloem_stats, y = CrossSections[c("CS_id", "age", "plant_id")])

  phloem_cell_diameter <- phloem_stats %>% filter(type == "cell_diameter")
  phloem_proportion <- phloem_stats %>% filter(type == "proportion")
  phloem_n_layers <- phloem_stats %>% filter(type == "n_layers")

  Ph1 <- anatomy_plot(phloem_cell_diameter, Title = "Phloem | cell_diameter")
  Ph2 <- anatomy_plot(phloem_n_layers, Title = "Phloem | n_layers")
  Ph3 <- anatomy_plot(phloem_proportion, Title = "Phloem | proportion")

  # PERICYCLE
  pericycle_stats <- anatomy2 %>% filter(name == "pericycle")
  pericycle_stats <- merge(x = pericycle_stats, y = CrossSections[c("CS_id", "age", "plant_id")])
  pericycle_cell_diameter <- pericycle_stats %>% filter(type == "cell_diameter")
  PC1 <- anatomy_plot(pericycle_cell_diameter, Title = "Pericycle | cell_diameter")

  # ENDODERMIS
  endodermis_stats <- anatomy2 %>% filter(name == "endodermis")
  endodermis_stats <- merge(x = endodermis_stats, y = CrossSections[c("CS_id", "age", "plant_id")])
  endodermis_cell_diameter <- endodermis_stats %>% filter(type == "cell_diameter")
  Endo1 <- anatomy_plot(endodermis_cell_diameter, Title = "Endodermis | cell_diameter")

  # CORTEX
  cortex_stats <- anatomy2 %>% filter(name == "cortex")
  cortex_stats <- merge(x = cortex_stats, y = CrossSections[c("CS_id", "age", "plant_id")])
  cortex_n_layers <- cortex_stats %>% filter(type == "n_layers")
  cortex_cell_diameter <- cortex_stats %>% filter(type == "cell_diameter")
  C1 <- anatomy_plot(cortex_n_layers, Title = "Cortex | n_layers")
  C2 <- anatomy_plot(cortex_cell_diameter, Title = "Cortex | cell_diameter")

  # EXODERMIS
  exodermis_stats <- anatomy2 %>% filter(name == "exodermis")
  exodermis_stats <- merge(x = exodermis_stats, y = CrossSections[c("CS_id", "age", "plant_id")])
  exodermis_cell_diameter <- exodermis_stats %>% filter(type == "cell_diameter")

  Exo1 <- anatomy_plot(exodermis_cell_diameter, Title = "Exodermis | cell_diameter")

  # EPIDERMIS
  epidermis_stats <- anatomy2 %>% filter(name == "epidermis")
  epidermis_stats <- merge(x = epidermis_stats, y = CrossSections[c("CS_id", "age", "plant_id")])
  epidermis_cell_diameter <- epidermis_stats %>% filter(type == "cell_diameter")
  Epi1 <- anatomy_plot(epidermis_cell_diameter, Title = "Epidermis | cell_diameter")

  #===============================================================================
  #===============================================================================
  legend <- get_legend(X1 + labs(color = "Plant ID") + theme(legend.position = "bottom"))
  p <- plot_grid(X2+ theme(legend.position = "none"),
                 X3+ theme(legend.position = "none"),
                 X4+ theme(legend.position = "none"),
                 S1+ theme(legend.position = "none"),
                 S2+ theme(legend.position = "none"),
                 S3+ theme(legend.position = "none"),
                 S4+ theme(legend.position = "none"),
                 Ph1+ theme(legend.position = "none"),
                 Ph2+ theme(legend.position = "none"),
                 Ph3+ theme(legend.position = "none"),
                 PC1+ theme(legend.position = "none"),
                 Endo1+ theme(legend.position = "none"),
                 C1+ theme(legend.position = "none"),
                 C2+ theme(legend.position = "none"),
                 Exo1+ theme(legend.position = "none"),
                 Epi1+ theme(legend.position = "none"),
                 # legend,
                 theme_bw(),
                 ncol = 4)

  p2 <- plot_grid(p + theme(plot.margin=unit(c(0,0,-19,0), "cm")),
                  legend,
                  ncol = 1)

  return(p2)}
