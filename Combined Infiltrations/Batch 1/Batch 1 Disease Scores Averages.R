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

# library(agricolae)
# library(agricolaeplotr)
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
Path_Assay <- subset(Path_Assay, Treatment != "Ctrl.HP.I")
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
  ld.mean <- mean(as.numeric(Data_Sub$X5.dpi.Leaf.Damage))
  print(treatments[i])
  print(ld.mean)
  LD.Averages$`Leaf Damage Averages`[i] <- ld.mean
}


##Plot Data Here
#Standard Error
# Standard Error for  X5.dpi.Leaf.Damage
for(i in 1:x){
  Data_Sub <- subset(Path_Assay, Treatment == treatments[i])
  Ld.SE <- sd(Data_Sub$X5.dpi.Leaf.Damage, na.rm = TRUE) / sqrt(length(Data_Sub$X5.dpi.Leaf.Damage))
  LD.Averages$SE[i] <- Ld.SE
}

# Convert data and SE values to long format
# If you haven't already installed reshape2
# install.packages("reshape2")

#add cld letters to LD.averages
#Order both LD.Averages and disease_treatments_cld in alphabetical order

LD.Averages <- LD.Averages %>%
  arrange(Treatments)

disease_treatments_cld <- disease_treatments_cld %>%
  arrange(`Group`)

LD.Averages <- cbind(LD.Averages, disease_treatments_cld$Letter)
column_names <- c("Treatments", "Leaf Damage Averages", "SE", "CLD")
colnames(LD.Averages) <- column_names

# count the number of records for each treatment;
treatment_counts <- table(Path_Assay$Treatment)

# Display the counts
print(treatment_counts)

# Plot with CLD labels
# To make sure ggplot2 treats the treatments in the correct order and discrete categories, convert it to factor like this:
LD.Averages$Treatments <- unlist(LD.Averages$Treatments)
LD.Averages$Treatments <- factor(LD.Averages$Treatments)
#ggplot needs a character vector for axes values but directly converting Treatments to a factor causes ggplot to interpret Treatments as one value
# so the column needs to be converted into characters data type first and then converted to a factor - making Treatments a character vector
# Plot +
LD_plot <- ggplot(LD.Averages, aes(x = Treatments, y = `Leaf Damage Averages`, fill = Treatments)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = `Leaf Damage Averages` - SE, ymax = `Leaf Damage Averages` + SE), width = 0.2) +
  geom_text(aes(label = CLD, y = `Leaf Damage Averages` + SE + 1), size = 5) +
  theme_minimal() +
  labs(y = "Average Leaf Damage Score", title = "Col-0 Batch 1 Average Leaf Damage Scores") +
  scale_fill_brewer(palette = "Blues")  # nicer than all-blue

show(LD_plot)

## Chlorosis

#get the number of treatments
x <- length(treatments)
#Create Chlorosis assay (LD) Averages data frame
# Define the column names
column_names <- c("Treatments", "Chlorosis Averages")

# Create an empty data frame with the specified column names
Chlorosis.Averages <- data.frame(matrix(ncol = length(column_names), nrow = x))
colnames(Chlorosis.Averages) <- column_names
#add the treatments to the Averages Data frame
Chlorosis.Averages$Treatments <- as.data.frame(treatments)
# Display the empty data frame
print(Chlorosis.Averages)
for(i in 1:x){
  Data_Sub <- subset(Path_Assay, Treatment == treatments[i])
  Data_Sub
  cl.mean <- mean(as.numeric(Data_Sub$Chlorosis))
  print(cl.mean)
  Chlorosis.Averages$`Chlorosis Averages`[i] <- cl.mean
}


##Plot Data Here
#Standard Error
# Standard Error for  Chlorosis
for(i in 1:x){
  Data_Sub <- subset(Path_Assay, Treatment == treatments[i])
  cl.SE <- sd(Data_Sub$Chlorosis, na.rm = TRUE) / sqrt(length(Data_Sub$Chlorosis))
  Chlorosis.Averages$SE[i] <- cl.SE
}


# Load required library
library(ggplot2)
library(reshape2)



# Convert data and SE values to long format
# If you haven't already installed reshape2
# install.packages("reshape2")


# add cld letters to LD.averages

Chlorosis.Averages <- Chlorosis.Averages %>%
  arrange(Treatments)

chlorosis_cld <- chlorosis_cld %>%
  arrange(`Group`)

Chlorosis.Averages <- cbind(Chlorosis.Averages, chlorosis_cld$Letter)
column_names <- c("Treatments", "Chlorosis Averages", "SE", "CLD")
colnames(Chlorosis.Averages) <- column_names

# Count the number of records for each Treatment type
ld_treatment_counts <- table(LD_Assay$Treatment)
cl_treatment_counts <- table(Chlorosis_Assay$Treatment)

# Plot with CLD labels
# To make sure ggplot2 treats the treatments in the correct order and discrete categories, convert it to factor like this:
Chlorosis.Averages$Treatments <- unlist(Chlorosis.Averages$Treatments)
Chlorosis.Averages$Treatments <- factor(Chlorosis.Averages$Treatments)
#ggplot needs a character vector for axes values but directly converting Treatments to a factor causes ggplot to interpret Treatments as one value
# so the column needs to be converted into characters data type first and then converted to a factor - making Treatments a character vector

# Plot +
CL_plot <- ggplot(Chlorosis.Averages, aes(x = Treatments, y = `Chlorosis Averages`, fill = Treatments)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = `Chlorosis Averages` - SE, ymax = `Chlorosis Averages` + SE), width = 0.2) +
  geom_text(aes(label = CLD, y = `Chlorosis Averages` + SE + 1), size = 5) +
  theme_minimal() +
  labs(y = "Average Chlorosis Score", title = "Col-0 Batch 7 Average Chlorosis Scores 5 dpi with Pseudomonas syringae") +
  scale_fill_brewer(palette = "PuBu")  # nicer than all-blue

show(CL_plot)