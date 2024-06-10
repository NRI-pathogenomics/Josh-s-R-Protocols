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
Si_Fi <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Si Fi Data/Si Fi results.csv", 
                  header = TRUE, sep = ",", quote = "\"",
                  dec = ".", fill = TRUE, comment.char = "")
# Extract Genome comparison type ("V.dahliae JR2" or "Col-0")
data <- data %>%
  mutate(Comparison = ifelse(grepl("V.dahliae JR2", Genome), "V.dahliae JR2", "Col-0"))

# Ensure efficient hits are numeric and replace NA with 0
data$Efficient.siRNA.hits[is.na(data$Efficient.siRNA.hits)] <- 0

# Convert Genome to a factor to keep the order
data$Genome <- factor(data$Genome, levels = unique(data$Genome))

# Plot the bar chart
ggplot(data, aes(x = Genome, fill = Comparison)) +
  geom_bar(aes(y = Total.siRNA.hits), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Efficient.siRNA.hits), stat = "identity", position = "dodge", color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("V.dahliae JR2" = "blue", "Col-0" = "green")) +
  labs(title = "Total vs Efficient siRNA Hits per Genome",
       x = "Genome",
       y = "Number of siRNA Hits",
       fill = "Comparison") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))