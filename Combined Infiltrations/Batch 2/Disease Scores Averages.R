#Disease Assay Average Scores 
#Batch 2
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

Path_Assay <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 2/Batch 2 Scores.csv", 
                       header = TRUE, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE, comment.char = "")
# Pre-process the data
Path_Assay <- na.omit(Path_Assay)
Path_Assay <- subset(Path_Assay, select = -c(Random, Block.Rep)) #remove randomized block design calculations
Path_Assay$Treatment <- gsub("-", ".", Path_Assay$Treatment)
## Calculate the Average Leaf Damage of each treatment
# take the individual treatment names
treatments <- unique(Path_Assay$Treatment)
#get the number of treatments
x <- length(treatments)
#Create Leaf Damage (LD) Averages data frame
# Define the column names
column_names <- c("Treatments", "Leaf Damage Averages")

# Create an empty data frame with the specified column names
LD.Averages <- data.frame(matrix(ncol = length(column_names), nrow = x))
colnames(LD.Averages) <- column_names
#add the treatments to the Averages Data frame
LD.Averages$Treatments <- as.data.frame(treatments)
# Display the empty data frame
print(LD.Averages)
for(i in 1:x){
  Data_Sub <- subset(Path_Assay, Treatment == treatments[i])
  Data_Sub
  ld.mean <- mean(as.numeric(Data_Sub$Leaf.Damage))
  LD.Averages$`Leaf Damage Averages`[i] <- ld.mean
}


##Plot Data Here
#Standard Error
# Standard Error for  Leaf.Damage
for(i in 1:x){
  Data_Sub <- subset(Path_Assay, Treatment == treatments[i])
  Ld.SE <- sd(Data_Sub$Leaf.Damage, na.rm = TRUE) / sqrt(length(Data_Sub$Leaf.Damage))
  LD.Averages$SE[i] <- Ld.SE
}


# Load required library
library(ggplot2)
library(reshape2)



# Convert data and SE values to long format
# If you haven't already installed reshape2
install.packages("reshape2")

# Load the library
library(reshape2)

#add cld letters to LD.averages
#Order both LD.Averages and disease_treatments_cld in alphabetical order
LD.Averages <- LD.Averages %>%
  arrange(Treatments)
disease_treatments_cld <- disease_treatments_cld %>%
  arrange(Group)
LD.Averages <- as.data.frame(LD.Averages)
disease_treatments_cld <- as.data.frame(disease_treatments_cld)
LD.Averages$CLD <- disease_treatments_cld$Letter
# # Melt the data
# LD.long <- melt(LD.Averages, id.vars = "Treatments",
#                 variable.name = "Measure", value.name = "Value")
# SE.long <- melt(SE.Values, id.vars = "SE", variable.name = "Metric", value.name = "SE")
# #merge the data
# LD.plot.data <- cbind(LD.long, SE = SE.long$SE)
# disease_treatments_cld


# Plot with CLD labels
# To make sure ggplot2 treats the treatments in the correct order and discrete categories, convert it to factor like this:
LD.Averages$Treatments <- as.character(treatments)
LD.Averages$Treatments <- factor(LD.Averages$Treatments, levels = unique(LD.Averages$Treatments))
#ggplot needs a character vector for axes values but directly converting Treatments to a factor causes ggplot to interpret Treatments as one value
# so the column needs to be converted into characters data type first and then converted to a factor - making Treatments a character vector
# Plot +
LD_plot <- ggplot(LD.Averages, aes(x = Treatments, y = `Leaf Damage Averages`, fill = Treatments)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = `Leaf Damage Averages` - SE, ymax = `Leaf Damage Averages` + SE), width = 0.2) +
  geom_text(aes(label = CLD, y = `Leaf Damage Averages` + SE + 1), size = 5) +
  theme_minimal() +
  labs(y = "Average Leaf Damage Score", title = "Col-0 Batch 2 Average Leaf Damage Scores") +
  scale_fill_brewer(palette = "Blues")  # nicer than all-blue

show(LD_plot)
