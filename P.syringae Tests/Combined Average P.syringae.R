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

Path_Assay <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/P.syringae Tests/P.syringae scores.csv", 
                       header = TRUE, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE, comment.char = "")
Path_Assay <- setNames(Path_Assay, nm=c("Replicate", "Treatment", "Score", "Chlorosis"))
Path_Assay <- na.omit(Path_Assay)
Path_Assay$Treatment <- as.factor(Path_Assay$Treatment)

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

##Normality
Score_Normality <- shapiro.test(Path_Assay$Score)
# if the P.value for the shapiro wilkes test is below 0.05 this indicates the results are not normally distributed
# if results are not normally distributed then they need to be analysed by a non-parametric test
if(Score_Normality[2] < 0.05){
  Score_KW <- kruskal.test(Score ~ Treatment, data = Path_Assay)
}
Chlorosis_Normality <- shapiro.test(Path_Assay$Chlorosis)
if(Chlorosis_Normality[2] < 0.05){
  Chlorosis_KW <- kruskal.test(Chlorosis ~ Treatment, data = Path_Assay)
}

## post-hoc test
#if the non-parametric results have a p-value below 0.05 then the Scores and Chlorosis levels do differ significantly 
# this requires a post-hoc Dunn test for multiple comparisons
# Pairwise comparisons for Score
scores_dunn_test <- dunn.test(Path_Assay$Score, g=interaction(Path_Assay$Treatment), method="bonferroni")

# Pairwise comparisons for Chlorosis
chlorosis_dunn_test <- dunn.test(Path_Assay$Chlorosis, g=interaction(Path_Assay$Treatment), method="bonferroni")

## cld
# Convert Dunn's test output to a data frame with proper naming
scores_dunn_df <- data.frame(
  Comparison = scores_dunn_test$comparisons,  # Extract comparisons
  P.adj = scores_dunn_test$P.adjusted        # Extract adjusted p-values
)

chlorosis_dunn_df <- data.frame(
  Comparison = chlorosis_dunn_test$comparisons,
  P.adj = chlorosis_dunn_test$P.adjusted
)

# Ensure no NA values in comparisons
scores_dunn_df <- na.omit(scores_dunn_df)
chlorosis_dunn_df <- na.omit(chlorosis_dunn_df)

# CLD for Scores
score_cld <- cldList(P.adj ~ Comparison, data = scores_dunn_df, threshold = 0.05)
print(score_cld)

# CLD for Chlorosis
chlorosis_cld <- cldList(P.adj ~ Comparison, data = chlorosis_dunn_df, threshold = 0.05)
print(chlorosis_cld)


## Plot
# Convert CLD results into a data frame
score_cld_df <- data.frame(Condition = c("Mock", "Inno"), Metric = "Score", CLD = score_cld$Letter)
chlorosis_cld_df <- data.frame(Condition = c("Mock", "Inno"), Metric = "Chlorosis", CLD = chlorosis_cld$Letter)

# Combine the two CLD dataframes
cld_df <- rbind(score_cld_df, chlorosis_cld_df)

# Merge CLD labels into the long-format data
data_long <- left_join(data_long, cld_df, by = c("Condition", "Metric"))

# Plot with CLD labels
ggplot(data_long, aes(x = Condition, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), width = 0.2, 
                position = position_dodge(0.9)) +
  geom_text(aes(label = CLD, y = Value + SE + 1),  # Adjust position slightly above bars
            position = position_dodge(0.9), size = 5) +
  theme_minimal() +
  labs(y = "Average Score", title = "Scores and Chlorosis with Standard Error & CLD") +
  scale_fill_manual(values = c("red", "blue"))