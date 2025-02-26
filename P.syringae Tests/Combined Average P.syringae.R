#AUDPC P.syrinage data
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
Path_Assay <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/P.syringae Tests/P.syringae scores.csv", 
                       header = TRUE, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE, comment.char = "")
Path_Assay <- setNames(Path_Assay, nm=c("Replicate", "Treatment", "Score", "Chlorosis"))


if(file.exists("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/P.syringae Tests/Mock Averaged Results")==FALSE){
  file.create("Mock Averaged Results")
}

if(file.exists("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/P.syringae Tests/Inoculated Averaged Results")==FALSE){
  file.create("Innoculated Averaged Results")
}
##Subsetting the Data
Inno_Sub <- as.data.frame(subset(Path_Assay, Treatment=="Inoculated"))
Inno_Sub <- na.omit(Inno_Sub)
Mock_Sub <- as.data.frame(subset(Path_Assay, Treatment=="Mock"))
Mock_Sub <- na.omit(Mock_Sub)
## Reformat columns
Mock_Sub$Score <- as.numeric(Mock_Sub$Score)
Mock_Sub$Chlorosis <- as.numeric(Mock_Sub$Chlorosis)

Inno_Sub$Score <- as.numeric(Inno_Sub$Score)
Inno_Sub$Chlorosis <- as.numeric(Inno_Sub$Chlorosis)

Mock_Scores_Average <- colMeans(Mock_Sub[3:4], na.rm = TRUE)
Inno_Scores_Average <- colMeans(Inno_Sub[3:4], na.rm = TRUE)
##Plot Data Here
#Standard Error
# Standard Error for Mock Score
Mock_Score_SE <- sd(Mock_Sub$Score, na.rm = TRUE) / sqrt(length(Mock_Sub$Score))

# Standard Error for Mock Chlorosis
Mock_Chlorosis_SE <- sd(Mock_Sub$Chlorosis, na.rm = TRUE) / sqrt(length(Mock_Sub$Chlorosis))

# Load required library
library(ggplot2)
library(reshape2)

# Define your average values
data <- data.frame(
  Condition = c("Mock", "Inno"),
  Score = c(Mock_Scores_Average[1], Inno_Scores_Average[1]),
  Chlorosis = c(Mock_Scores_Average[2], Inno_Scores_Average[2])
)

# Define your standard error values
SE_values <- data.frame(
  Condition = c("Mock", "Inno"),
  Score = c(Mock_Score_SE, Inno_Score_SE),
  Chlorosis = c(Mock_Chlorosis_SE, Inno_Chlorosis_SE)
)

# Convert data and SE values to long format
data_long <- melt(data, id.vars = "Condition", variable.name = "Metric", value.name = "Value")
SE_long <- melt(SE_values, id.vars = "Condition", variable.name = "Metric", value.name = "SE")

# Merge standard error values into the main data
data_long <- merge(data_long, SE_long, by = c("Condition", "Metric"))

# Plot with error bars
ggplot(data_long, aes(x = Condition, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), width = 0.2, 
                position = position_dodge(0.9)) +
  theme_minimal() +
  labs(y = "Average Score", title = "Scores and Chlorosis with Standard Error") +
  scale_fill_manual(values = c("blue", "red"))
