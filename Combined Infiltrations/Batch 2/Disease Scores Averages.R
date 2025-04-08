#Disease Assay Average Scores 
#Batch 2
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
if(("FSA" %in% all_packages)==FALSE){
  install.packages("FSA")}
if(("dunn.test" %in% all_packages)==FALSE){
  install.packages("dunn.test")}
if(("rcompanion" %in% all_packages)==FALSE){
  install.packages("rcompanion")}

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
Path_Assay <- subset(Path_Assay, select = -c(Random, Block, Block.Rep)) #remove randomized block design calculations
Path_Assay <- subset(Path_Assay, Fungus.gnats == "N") #remove the rows where fungus gnat 

## Subset the Mock vs Inoculated replicates
Mock_Sub <- subset(Path_Assay, Treatment=="GFP-M")
Mock_Sub <- Mock_Sub[1:4]
Inno_Sub <- subset(Path_Assay, Infiltrated.with.P.syringae=="Y")
Inno_Sub <- Inno_Sub[1:4]

Mock_Scores_Average <- colMeans(Mock_Sub[3:4], na.rm = TRUE)
Inno_Scores_Average <- colMeans(Inno_Sub[3:4], na.rm = TRUE)
##Plot Data Here
#Standard Error
# Standard Error for  Leaf.Damage
Mock_LD_SE <- sd(Mock_Sub$Leaf.Damage, na.rm = TRUE) / sqrt(length(Mock_Sub$Leaf.Damage))
Inno_LD_SE <- sd(Inno_Sub$Leaf.Damage, na.rm = TRUE) / sqrt(length(Inno_Sub$Leaf.Damage))
# Standard Error for  Chlorosis
Mock_Chlorosis_SE <- sd(Mock_Sub$Chlorosis, na.rm = TRUE) / sqrt(length(Mock_Sub$Chlorosis))
Inno_Chlorosis_SE <- sd(Inno_Sub$Chlorosis, na.rm = TRUE) / sqrt(length(Inno_Sub$Chlorosis))
# Load required library
library(ggplot2)
library(reshape2)

# Define your average values
LD_data <- data.frame(
  Condition = c("Mock", "Inno"),
  Score = c(Mock_Scores_Average[1], Inno_Scores_Average[1]))
chlorosis_data <- data.frame(
  Condition = c("Mock", "Inno"),
  Chlorosis = c(Mock_Scores_Average[2], Inno_Scores_Average[2]))

# Define your standard error values
score_SE_values <- data.frame(
  Condition = c("Mock", "Inno"),
  Score = c(Mock_Score_SE, Inno_Score_SE)
)
chlorosis_SE_values <- data.frame(
  Condition = c("Mock", "Inno"),
  Chlorosis = c(Mock_Chlorosis_SE, Inno_Chlorosis_SE))

# Convert data and SE values to long format
LD_data_long <- melt(LD_data, id.vars = "Condition", variable.name = "Metric", value.name = "Value")
LD_SE_long <- melt(LD_SE_values, id.vars = "Condition", variable.name = "Metric", value.name = "SE")

chlorosis_data_long <- melt(chlorosis_data, id.vars = "Condition", variable.name = "Metric", value.name = "Value")
chlorosis_SE_long <- melt(chlorosis_SE_values, id.vars = "Condition", variable.name = "Metric", value.name = "SE")
# Merge standard error values into the main data
LD_data_long <- merge(LD_data_long, LD_SE_long, by = c("Condition", "Metric"))
chlorosis_data_long <- merge(chlorosis_data_long, chlorosis_SE_long, by = c("Condition", "Metric"))

##Normality
LD_Normality <- shapiro.test(Path_Assay$Leaf.Damage)
# if the P.value for the shapiro wilkes test is below 0.05 this indicates the results are not normally distributed
# if results are not normally distributed then they need to be analysed by a non-parametric test
if(LD_Normality[2] < 0.05){
  LD_KW <- kruskal.test(Leaf.Damage ~ Treatment, data = Path_Assay)
}
Chlorosis_Normality <- shapiro.test(Path_Assay$Chlorosis)
if(Chlorosis_Normality[2] < 0.05){
  Chlorosis_KW <- kruskal.test(Chlorosis ~ Treatment, data = Path_Assay)
}

## post-hoc test
#if the non-parametric results have a p-value below 0.05 then the Scores and Chlorosis levels do differ significantly 
# this requires a post-hoc Dunn test for multiple comparisons
# Pairwise comparisons for Score
LD_dunn_test <- dunn.test(Path_Assay$Leaf.Damage, g=interaction(Path_Assay$Treatment), method="bonferroni")

# Pairwise comparisons for Chlorosis
chlorosis_dunn_test <- dunn.test(Path_Assay$Chlorosis, g=interaction(Path_Assay$Treatment), method="bonferroni")

## cld
# Convert Dunn's test output to a data frame with proper naming
LD_dunn_df <- data.frame(
  Comparison = LD_dunn_test$comparisons,  # Extract comparisons
  P.adj = LD_dunn_test$P.adjusted        # Extract adjusted p-values
)

chlorosis_dunn_df <- data.frame(
  Comparison = chlorosis_dunn_test$comparisons,
  P.adj = chlorosis_dunn_test$P.adjusted
)

# Ensure no NA values in comparisons
LD_dunn_df <- na.omit(LD_dunn_df)
chlorosis_dunn_df <- na.omit(chlorosis_dunn_df)

# CLD for Scores
LD_cld <- cldList(P.adj ~ Comparison, data = LD_dunn_df, threshold = 0.05)
print(LD_cld)

# CLD for Chlorosis
chlorosis_cld <- cldList(P.adj ~ Comparison, data = chlorosis_dunn_df, threshold = 0.05)
print(chlorosis_cld)


## Plot
# Convert CLD results into a data frame
LD_cld_df <- data.frame(Condition = c("Mock", "Inno"), Metric = "Leaf.Damage", CLD = score_cld$Letter)
chlorosis_cld_df <- data.frame(Condition = c("Mock", "Inno"), Metric = "Chlorosis", CLD = chlorosis_cld$Letter)

# Combine the results to their respective CLD dataframes
LD_and_cld_df <- left_join(score_data_long, score_cld_df, by = c("Condition", "Metric"))
chlorosis_and_cld_df <- left_join(chlorosis_data_long, chlorosis_cld_df, by = c("Condition", "Metric"))
# Plot with CLD labels
LD_plot <- ggplot(LD_and_cld_df, aes(x = Condition, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), width = 0.2, 
                position = position_dodge(0.9)) +
  geom_text(aes(label = CLD, y = Value + SE + 1),  # Adjust position slightly above bars
            position = position_dodge(0.9), size = 5) +
  theme_minimal() +
  labs(y = "Average Score", title = "Average Disease Index Scores 0-10 for Col-0 Infected with P.syringae") +
  scale_fill_manual(values = c("blue"))

show(LD_plot)