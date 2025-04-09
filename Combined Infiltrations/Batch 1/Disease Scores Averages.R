#Disease Assay Average Scores 
#Batch 1
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

Path_Assay <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 1/Batch 1 Scores.csv", 
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
column_names <- c("SE")
Ld.SE.values <- list()
for(i in 1:x){
  Data_Sub <- subset(Path_Assay, Treatment == treatments[i])
  Ld.SE <- sd(Data_Sub$Leaf.Damage, na.rm = TRUE) / sqrt(length(Data_Sub$Leaf.Damage))
  Ld.SE.values <- c(Ld.SE.values, Ld.SE)
}
#Convert the Leaf damage SE values to the LD.Averages table
SE.Values <- data.frame(SE = unlist(Ld.SE.values))

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
# Melt the data
LD.long <- melt(LD.Averages, id.vars = "Treatments",
                variable.name = "Measure", value.name = "Value")
SE.long <- melt(SE.Values, id.vars = "SE", variable.name = "Metric", value.name = "SE")
#merge the data
LD.plot.data <- cbind(LD.long, SE = SE.long$SE)
disease_treatments_cld


# # Plot with CLD labels
# LD_plot <- ggplot(LD_and_cld_df, aes(x = Condition, y = Value, fill = Metric)) +
#   geom_bar(stat = "identity", position = position_dodge(), color = "black") +
#   geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), width = 0.2, 
#                 position = position_dodge(0.9)) +
#   geom_text(aes(label = CLD, y = Value + SE + 1),  # Adjust position slightly above bars
#             position = position_dodge(0.9), size = 5) +
#   theme_minimal() +
#   labs(y = "Average Score", title = "Average Disease Index Scores 0-10 for Col-0 Infected with P.syringae") +
#   scale_fill_manual(values = c("blue"))
# 
# show(LD_plot)