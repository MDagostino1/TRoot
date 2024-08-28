# This code process anatomy2 database to build regression functions for each GRANAR parameters
# TO UPDATE : save coefficients in functions instead of calling wild lm objects

head(anatomy2)
anatomy2E <- anatomy2 %>% merge(CrossSections[c("CS_id", "age")])

#===============================================================================
# Xylem
#===============================================================================

##Xylem n files

xylem_n_files <- function(x){
  return(2.)
}

## Xylem n cells

xylem_n_cells_df <- anatomy2E %>% filter(param_id == "xylem_n_cells")
xylem_n_cells_df %>% ggplot(aes(x = age, y = value)) + geom_point()

xylem_n_cells_lm1 <- lm(xylem_n_cells_df%>% filter(age < 20), formula = value ~ age)
xylem_n_cells_lm2 <- lm(xylem_n_cells_df%>% filter(age >= 20), formula = ln(value) ~ age)

summary(xylem_n_cells_lm1)
summary(xylem_n_cells_lm2)

xylem_n_cells <- function(x){
  if(x < 25){
    y <- predict(xylem_n_cells_lm1, 
                 newdata = data.frame(age = x)) +
      rnorm(1, mean = 0, sd = sd(residuals(xylem_n_cells_lm1)))
  } else{
    y <- exp(predict(xylem_n_cells_lm2, 
                     newdata = data.frame(age = x)) +
               rnorm(1, mean = 0, sd = sd(residuals(xylem_n_cells_lm2))))
  }
  
  return(round(y))
}

## Xylem max size

xylem_max_size_df <- anatomy2E %>% filter(param_id == "xylem_max_size")
xylem_max_size_df %>% ggplot(aes(x = age, y = value)) + geom_point()
xylem_max_size_lm <- lm(xylem_max_size_df, formula = value ~ age)
summary(xylem_max_size_lm)

xylem_max_size <- function(x){
  y <- predict(xylem_max_size_lm, 
               newdata = data.frame(age = x)) + 
    rnorm(1, mean = 0, sd = sd(residuals(xylem_max_size_lm)))
  return(abs(y))
}

## Xylem cell diameter

xylem_cell_diameter_df <- anatomy2E %>% filter(param_id == "xylem_cell_diameter")
xylem_cell_diameter_df %>% ggplot(aes(x = age, y = value)) + geom_point()
xylem_cell_diameter_lm <- lm(xylem_cell_diameter_df, formula = value ~ age)
summary(xylem_cell_diameter_lm)

xylem_cell_diameter <- function(x){
  y <- predict(xylem_cell_diameter_lm, 
               newdata = data.frame(age = x)) + 
    rnorm(1, mean = 0, sd = sd(residuals(xylem_cell_diameter_lm)))
  return(y)
}

#===============================================================================
# Stele
#===============================================================================

## Stele layer diameter


stele_layer_diameter_df <- anatomy2E %>% filter(param_id == "stele_layer_diameter")
stele_layer_diameter_df %>% ggplot(aes(x = age, y = value)) + geom_point()
stele_layer_diameter_lm1 <- lm(stele_layer_diameter_df %>% filter(age < 25), formula = value ~ age)
stele_layer_diameter_lm2 <- lm(stele_layer_diameter_df %>% filter(age >= 25), formula = ln(value) ~ age)
summary(stele_layer_diameter_lm1)
summary(stele_layer_diameter_lm2)

stele_layer_diameter <- function(x){
  if(x < 25){
    y <- predict(stele_layer_diameter_lm1, 
                 newdata = data.frame(age = x)) + 
      rnorm(1, mean = 0, sd = sd(residuals(stele_layer_diameter_lm1)))
  }else if(x >= 25){
    y <- exp(predict(stele_layer_diameter_lm2, 
                     newdata = data.frame(age = x)) + 
               rnorm(1, mean = 0, sd = sd(residuals(stele_layer_diameter_lm2))))
    
    if(y < 0.2){
      y <- 0.2
    }
  }
  
  return(y)
}

## Stele cell diameter

stele_cell_diameter_df <- anatomy2E %>% filter(param_id == "stele_cell_diameter")
stele_cell_diameter_df %>% ggplot(aes(x = age, y = value)) + geom_point()
stele_cell_diameter_lm1 <- lm(stele_cell_diameter_df %>% filter(age < 25), formula = value ~ age)
stele_cell_diameter_lm2 <- lm(stele_cell_diameter_df %>% filter(age >= 25), formula = ln(value) ~ age)
summary(stele_cell_diameter_lm1)
summary(stele_cell_diameter_lm2)

stele_cell_diameter <- function(x){
  if(x < 25){
    y <- predict(stele_cell_diameter_lm1, 
                 newdata = data.frame(age = x)) + 
      rnorm(1, mean = 0, sd = sd(residuals(stele_cell_diameter_lm1)))
  }else if(x >= 25){
    y <- exp(predict(stele_cell_diameter_lm2, 
                     newdata = data.frame(age = x)) + 
               rnorm(1, mean = 0, sd = sd(residuals(stele_cell_diameter_lm2))))
    
    if(y < 0.01){
      y <- 0.01
    }
  }
  
  return(y)
}

#===============================================================================
## Stele n layers
#===============================================================================

stele_n_layers_df <- anatomy2E %>% filter(param_id == "stele_n_layers")
stele_n_layers_df %>% ggplot(aes(x = age, y = value)) + geom_point()
stele_n_layers_lm <- lm(stele_n_layers_df, formula = ln(value) ~ age)
summary(stele_n_layers_lm)

stele_n_layers <- function(x){
  y <- exp(predict(stele_n_layers_lm, 
                   newdata = data.frame(age = x)) + 
             rnorm(1, mean = 0, sd = sd(residuals(stele_n_layers_lm)
             ))
  )
  return(y)
}

#===============================================================================
## Stele SD
#===============================================================================

stele_SD_df <- anatomy2E %>% filter(param_id == "stele_SD")
stele_SD_df %>% ggplot(aes(x = age, y = value)) + geom_point()
stele_SD_lm1 <- lm(stele_SD_df %>% filter(age < 25), formula = value ~ age)
stele_SD_lm2 <- lm(stele_SD_df %>% filter(age >= 25), formula = ln(value) ~ age)
summary(stele_SD_lm1)
summary(stele_SD_lm2)

stele_SD <- function(x){
  if(x < 25){
    y <- predict(stele_SD_lm1, 
                 newdata = data.frame(age = x)) + 
      rnorm(1, mean = 0, sd = sd(residuals(stele_SD_lm1)))
  }else if(x >= 25){
    y <- exp(predict(stele_SD_lm2, 
                     newdata = data.frame(age = x)) + 
               rnorm(1, mean = 0, sd = sd(residuals(stele_SD_lm2))))
    
    if(y < 0.001){
      y<- 0.001
    }
  }
  
  return(y)
}

#===============================================================================
# Phloem
#===============================================================================

## Phloem n layers

phloem_n_layers_df <- anatomy2E %>% filter(param_id == "phloem_n_layers")
phloem_n_layers_df %>% ggplot(aes(x = age, y = value)) + geom_point()
phloem_n_layers_lm <- lm(phloem_n_layers_df %>% filter(age > 20 & value != 0), formula = ln(abs(value)) ~ age)
summary(phloem_n_layers_lm)

phloem_n_layers <- function(x){
  if(x <= 7){
    y <- 0.
  }else if(x > 7 & x < 20){
    y <- 1.
  }else if(x >= 20){
    y <- exp(predict(phloem_n_layers_lm, 
                     newdata = data.frame(age = x)) + 
               rnorm(1, mean = 0, sd = sd(residuals(phloem_n_layers_lm))))
  }
  return(round(abs(y)))
}

## Phloem cell diameter

phloem_cell_diameter_df <- anatomy2E %>% filter(param_id == "phloem_cell_diameter")
phloem_cell_diameter_df %>% ggplot(aes(x = age, y = value)) + geom_point()
phloem_cell_diameter_lm <- lm(phloem_cell_diameter_df, formula = value ~ 1)
summary(phloem_cell_diameter_lm)

phloem_cell_diameter <- function(x){
  
  y <- predict(phloem_cell_diameter_lm, 
               newdata = data.frame(age = x)) + 
    rnorm(1, mean = 0, sd = sd(residuals(phloem_cell_diameter_lm)))
  
  return(abs(y))
}

## Phloem proportion

phloem_proportion_df <- anatomy2E %>% filter(param_id == "phloem_proportion")
phloem_proportion_df %>% ggplot(aes(x = age, y = value)) + geom_point()
phloem_proportion_lm <- lm(phloem_proportion_df %>% filter(age > 7 & value > 0), formula = value ~ age)
summary(phloem_proportion_lm)

phloem_proportion <- function(x){
  if(x <= 7){
    y <- 0.
  }else if(x > 7){
    y <- predict(phloem_proportion_lm, 
                 newdata = data.frame(age = x)) + 
      rnorm(1, mean = 0, sd = sd(residuals(phloem_proportion_lm)))
  }
  return(y)
}

#==============================================================================
# Pericycle cell diameter
#===============================================================================

pericycle_cell_diameter_df <- anatomy2E %>% filter(param_id == "pericycle_cell_diameter")
pericycle_cell_diameter_df %>% ggplot(aes(x = age, y = value)) + geom_point()
pericycle_cell_diameter_lm <- lm(pericycle_cell_diameter_df, formula = value ~ 1)
summary(pericycle_cell_diameter_lm)

pericycle_cell_diameter <- function(x){
  y <- predict(pericycle_cell_diameter_lm, 
               newdata = data.frame(age = x)) + 
    rnorm(1, mean = 0, sd = sd(residuals(pericycle_cell_diameter_lm)))
  return(y)
}

#===============================================================================
# Endodermis cell diameter
#===============================================================================

endodermis_cell_diameter_df <- anatomy2E %>% filter(param_id == "endodermis_cell_diameter")
endodermis_cell_diameter_df %>% ggplot(aes(x = age, y = value)) + geom_point()
endodermis_cell_diameter_lm <- lm(endodermis_cell_diameter_df, formula = value ~ age)
summary(endodermis_cell_diameter_lm)

endodermis_cell_diameter <- function(x){
  y <- predict(endodermis_cell_diameter_lm, 
               newdata = data.frame(age = x)) + 
    rnorm(1, mean = 0, sd = sd(residuals(endodermis_cell_diameter_lm)))
  return(y)
}

#===============================================================================
# Cortex
#===============================================================================

## COrtex n layers

cortex_n_layers_df <- anatomy2E %>% filter(param_id == "cortex_n_layers")
cortex_n_layers_df %>% ggplot(aes(x = age, y = value)) + geom_point()
cortex_n_layers_lm <- lm(cortex_n_layers_df, formula = value ~ age)
summary(cortex_n_layers_lm)

cortex_n_layers <- function(x){
  y <- predict(cortex_n_layers_lm, 
               newdata = data.frame(age = x)) + 
    rnorm(1, mean = 0, sd = sd(residuals(cortex_n_layers_lm)))
  
  if(y < 1){
    y <- 1
  }
  return(round(y))
}

## Cortex cell diameter

cortex_cell_diameter_df <- anatomy2E %>% filter(param_id == "cortex_cell_diameter")
cortex_cell_diameter_df %>% ggplot(aes(x = age, y = value)) + geom_point()
cortex_cell_diameter_lm <- lm(cortex_cell_diameter_df, formula = value ~ age)
summary(cortex_cell_diameter_lm)

cortex_cell_diameter <- function(x){
  y <- predict(cortex_cell_diameter_lm, 
               newdata = data.frame(age = x)) + 
    rnorm(1, mean = 0, sd = sd(residuals(cortex_cell_diameter_lm)))
  
  return(y)
}

#===============================================================================
# Exodermis cell diameter
#===============================================================================

exodermis_cell_diameter_df <- anatomy2E %>% filter(param_id == "exodermis_cell_diameter")
exodermis_cell_diameter_df %>% ggplot(aes(x = age, y = value)) + geom_point()
exodermis_cell_diameter_lm <- lm(exodermis_cell_diameter_df, formula = value ~ age)
summary(exodermis_cell_diameter_lm)

exodermis_cell_diameter <- function(x){
  y <- predict(exodermis_cell_diameter_lm, 
               newdata = data.frame(age = x)) + 
    rnorm(1, mean = 0, sd = sd(residuals(exodermis_cell_diameter_lm)))
  
  return(y)
}

#===============================================================================
# Epidermis cell diamter
#===============================================================================

epidermis_cell_diameter_df <- anatomy2E %>% filter(param_id == "epidermis_cell_diameter")
epidermis_cell_diameter_df %>% ggplot(aes(x = age, y = value)) + geom_point()
epidermis_cell_diameter_lm <- lm(epidermis_cell_diameter_df, formula = value ~ age)
summary(epidermis_cell_diameter_lm)

epidermis_cell_diameter <- function(x){
  y <- predict(epidermis_cell_diameter_lm, 
               newdata = data.frame(age = x)) + 
    rnorm(1, mean = 0, sd = sd(residuals(epidermis_cell_diameter_lm)))
  
  return(y)
}


