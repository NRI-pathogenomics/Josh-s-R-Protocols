#Disease Assay Average Scores 
#Batch 3
# all_packages <- installed.packages()
# 
# if(("dplyr" %in% all_packages)==FALSE){
#   install.packages("dplyr")}
# if(("agricolae" %in% all_packages)==FALSE){
#   install.packages("agricolae")}
# if(("agricolaeplotr" %in% all_packages)==FALSE){
#   install.packages("agricolaeplotr")}
# if(("tidyverse" %in% all_packages)==FALSE){
#   install.packages("tidyverse")}
# if(("plotrix" %in% all_packages)==FALSE){
#   install.packages("plotrix")}
# if(("FSA" %in% all_packages)==FALSE){
#   install.packages("FSA")}
# if(("dunn.test" %in% all_packages)==FALSE){
#   install.packages("dunn.test")}
# if(("rcompanion" %in% all_packages)==FALSE){
#   install.packages("rcompanion")}

library(agricolae)
library(agricolaeplotr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tidyverse)
library(magrittr)
library(plotrix)
library(FSA)
library(dunn.test)
library(rcompanion)
library(readr)


Path_Assay <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 3/Batch 3 Scores.csv", 
                       header = TRUE, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE, comment.char = "")
## Leaf Damage
# Pre-process the data
Rot_Scores<- na.omit(Path_Assay)
Rot_Scores<- subset(Path_Assay, select = -c(Random, Block, Total.Leaves, Rotted.Leaves, Chlorotic.leaves, X..Chlorosis)) #remove randomized block design calculations
Rot_Scores$Treatment <- gsub("-", ".", Rot_Scores$Treatment)

## Change treatment names for clarity

Rot_Scores$Treatment[Rot_Scores$Treatment == "P.syringae.Only"] <- "Uninfiltrated"

Rot_Scores$Treatment[Rot_Scores$Treatment == "Mock"] <- "Mock.Solution"

Rot_Scores$Treatment[Rot_Scores$Treatment == "GFP.Only"] <- "GFP.Group.1"

Rot_Scores$Treatment[Rot_Scores$Treatment == "Both"] <- "GFP.Group.2"

# Convert % values stored as characters to numeric
Rot_Scores$X..of.leaf.rot <- parse_number(Rot_Scores$X..of.leaf.rot)
Rot_Scores$X..of.leaf.rot <- as.numeric(Rot_Scores$X..of.leaf.rot)
## Calculate the Average Leaf Damage of each treatment
# take the individual treatment names
treatments <- unique(Rot_Scores$Treatment)
#get the number of treatments
x <- length(treatments)
#Create Leaf Damage (LD) Averages data frame
# Define the column names
column_names <- c("Treatments", "Rot %")

# Create an empty data frame with the specified column names
Rot.Averages <- data.frame(matrix(ncol = length(column_names), nrow = x))
colnames(Rot.Averages) <- column_names
#add the treatments to the Averages Data frame
Rot.Averages$Treatments <- as.data.frame(treatments)
# Display the empty data frame
print(Rot.Averages)
for(i in 1:x){
  Data_Sub <- subset(Rot_Scores, Treatment == treatments[i])
  Data_Sub
  ld.mean <- mean(as.numeric(Data_Sub$`X..of.leaf.rot`))
  Rot.Averages$`Rot %`[i] <- ld.mean
}


##Plot Data Here
#Standard Error
# Standard Error for  Leaf.Damage
for(i in 1:x){
  Data_Sub <- subset(Rot_Scores, Treatment == treatments[i])
  Ld.SE <- sd(Data_Sub$`X..of.leaf.rot`, na.rm = TRUE) / sqrt(length(Data_Sub$`X..of.leaf.rot`))
  Rot.Averages$SE[i] <- Ld.SE
}


# Load required library
library(ggplot2)
library(reshape2)



# Convert data and SE values to long format
# If you haven't already installed reshape2

# add cld letters to Rot.Averages


Rot.Averages <- Rot.Averages %>%
  arrange(Treatments)

disease_treatments_cld <- disease_treatments_cld %>%
  arrange(`Group`)

Rot.Averages <- cbind(Rot.Averages, disease_treatments_cld$Letter)
column_names <- c("Treatments", "Leaf Rot %", "SE", "CLD")
colnames(Rot.Averages) <- column_names
# Plot with CLD labels
# To make sure ggplot2 treats the treatments in the correct order and discrete categories, convert it to factor like this:
Rot.Averages$Treatments <- unlist(Rot.Averages$Treatments)
Rot.Averages$Treatments <- factor(Rot.Averages$Treatments)
#ggplot needs a character vector for axes values but directly converting Treatments to a factor causes ggplot to interpret Treatments as one value
# so the column needs to be converted into characters data type first and then converted to a factor - making Treatments a character vector

# Plot +
LD_plot <- ggplot(Rot.Averages, aes(x = Treatments, y = `Leaf Rot %`, fill = Treatments)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = `Leaf Rot %` - SE, ymax = `Leaf Rot %` + SE), width = 0.2) +
  geom_text(aes(label = CLD, y = `Leaf Rot %` + SE + 4), size = 5) +
  theme_minimal() +
  labs(y = "Average Leaf Rot Score", title = "Col-0 Batch 3 Average Leaf Rot Scores") +
  scale_fill_brewer(palette = "Blues")  # nicer than all-blue

show(LD_plot)
