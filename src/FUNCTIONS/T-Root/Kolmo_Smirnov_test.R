
# Adrien Heymans
# October 2023

# Introduction to KS test

library(tidyverse)

# Creation of the dataset
data = tibble(Gen = sort(rep(c(1:3),1000)),
              Cell_length = c(rpois(n = 1000, 5)+runif(1000), 
                              rpois(n = 1000, 6)+runif(1000), 
                              rpois(n = 1000, 5.5)+runif(1000))*10)%>%
  filter(Cell_length >= 20)

# Density plot
data %>%
  ggplot(aes(Cell_length))+
  geom_density(aes(fill = factor(Gen)), alpha = 0.3)+
  viridis::scale_fill_viridis(discrete = T)+
  theme_classic()

# Histograms
data %>%
  ggplot(aes(Cell_length))+
  geom_histogram(aes(fill = factor(Gen)),position = "dodge", alpha = 0.7, binwidth = 10)+
  viridis::scale_fill_viridis(discrete = T)+
  theme_classic()

# Prep the data for frequency plot 
# create ECDF of data
cdf1 <- ecdf(data$Cell_length[data$Gen == 1]) # Gen 1 is the control
cdf2 <- ecdf(data$Cell_length[data$Gen == 2]) 
# find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(data$Cell_length[data$Gen == 1], data$Cell_length[data$Gen == 2]), 
              max(data$Cell_length[data$Gen == 1], data$Cell_length[data$Gen == 2]), 
              length.out=length(data$Cell_length[data$Gen == 1])) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 

# generate frequency plot 
ggplot()+
  stat_ecdf(aes(x = Cell_length, group = factor(Gen), color = factor(Gen)), size=1, 
            data = data%>%filter(Gen %in% c(1,2))) + # selection on the pair of genotypes that are analyze
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "dashed", color = "red", size = 1)+
  xlab("Cell length [µm]") +
  ylab("Cumulitive Distibution") +
  theme_classic()

# Calculate Stat
ks.test(data$Cell_length[data$Gen == 1], data$Cell_length[data$Gen == 2], alternative = "two.sided")






