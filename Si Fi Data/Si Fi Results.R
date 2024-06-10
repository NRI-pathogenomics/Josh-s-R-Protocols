#Si-Fi results
all_packages <- installed.packages()

if(("dplyr" %in% all_packages)==FALSE){
  install.packages("dplyr")}
if(("agricolae" %in% all_packages)==FALSE){
  install.packages("agricolae")}
if(("agricolaeplotr" %in% all_packages)==FALSE){
  install.packages("agricolaeplotr")}
if(("tidyverse" %in% all_packages)==FALSE){
  install.packages("tidyverse")}
if(("plotrix" %in% all_packages)==FALSE){
  install.packages("plotrix")}

library(agricolae)
library(agricolaeplotr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tidyverse)
library(magrittr)
library(plotrix)
Si_Fi <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Si Fi Data/Si Fi results total.csv", 
                  header = TRUE, sep = ",", quote = "\"",
                  dec = ".", fill = TRUE, comment.char = "")
# Reshape the data to long format
Si_Fi_long <- Si_Fi %>%
  pivot_longer(cols = c(Total.siRNA.hits, Efficient.siRNA.hits),
               names_to = "Hit_Type",
               values_to = "Hits") %>%
  mutate(Hit_Type = factor(Hit_Type, levels = c("Total.siRNA.hits", "Efficient.siRNA.hits"), 
                           labels = c("Total Hits", "Efficient Hits")))

# Create the bar plot with an outline and value labels
# scale_fill_manual helps to separate the datasets into different colors for total vs efficient hits 
# Create the dot plot
plott <- ggplot(Si_Fi_long, aes(x = Hairpin, y = Hits, color = interaction(Genome, Hit_Type))) +
  geom_point(aes(shape = Hit_Type, size = Hits), 
             position = position_jitter(width = 0.3, height = 0), 
             alpha = 0.7) +
  scale_color_manual(values = c("V.dahliae JR2.Total Hits" = "blue", "V.dahliae JR2.Efficient Hits" = "orange",
                                "Col-0.Total Hits" = "green", "Col-0.Efficient Hits" = "red")) +
  scale_size_continuous(range = c(2, 5)) +  # Adjust size range for visibility
  labs(title = "Total vs Efficient siRNA Hits per Hairpin",
       x = "Hairpin",
       y = "Number of siRNA Hits",
       color = "Type and Genome",
       shape = "Hit Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank())

# Display the plot
print(plott)