# aligned rank transform (ART)
### Step 1: Install and Load Necessary Packages
if(("ARTool" %in% all_packages)==FALSE){
  install.packages("ARTool")}
if(("emmeans" %in% all_packages)==FALSE){
  install.packages("emmeans")}
if(("multcompView" %in% all_packages)==FALSE){
  install.packages("multcompView") # for cld function
}
all_packages <- installed.packages()

if(("MASS" %in% all_packages)==FALSE){
  install.packages("MASS")}
if(("ggpmisc" %in% all_packages)==FALSE){
  install.packages("ggpmisc")}
if(("jmv" %in% all_packages)==FALSE){
  install.packages("jmv")}
if(("Rmisc" %in% all_packages)==FALSE){
  install.packages("Rmisc")}
if(("multcomp" %in% all_packages)==FALSE){
  install.packages("multcomp")}
if(("Hmisc" %in% all_packages)==FALSE){
  install.packages("Hmisc")}
if(("lattice" %in% all_packages)==FALSE){
  install.packages("lattice")}
if(("bestNormalize" %in% all_packages)==FALSE){
  install.packages("bestNormalize")}
library(MASS)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggpmisc)
library(jmv)
library(Rmisc)
library(emmeans) 
library(multcomp)
library(Hmisc)
library(lattice)
library(multcompView)
library(agricolae)
library(bestNormalize)
library(ARTool)
library(emmeans)
library(multcompView)

### Step 2: Create Example Data


data <- Results %>% select(Result_Genotype,Result_Type, Result_AUDPC)

rownames(data) <- NULL

#convert rows to required format

data$Result_AUDPC <- as.numeric(data$Result_AUDPC)
data$Result_Genotype <- as.factor(unlist(data$Result_Genotype))
data$Result_Type <- as.factor(unlist(data$Result_Type))
### Step 3: Perform Aligned Rank Transform

# Perform the aligned rank transform
art_model <- art(Result_AUDPC ~ Result_Genotype * Result_Type, data = data)
anova(art_model)

# After step 3 eemeans will not work - try this modifacation (you will need to modify this for your data)
#Extract estimates 
table<-art_model$estimated.effects
# Extract aligned ranks
data$aligned_ranks <- art_model$residuals + table$`Result_Genotype:Result_Type`
# Fit a linear model on the aligned ranks
lm_model <- lm(aligned_ranks ~ Result_Genotype * Result_Type, data = data)
base::summary(lm_model)

# Calculate estimated marginal means and perform pairwise comparisons
emmeans_results <- emmeans(lm_model, ~ Result_Genotype * Result_Type)
pairwise_comparisons <- pairs(emmeans_results)
base::summary(pairwise_comparisons)
# Obtain compact letter display (cld) to group Result_Types
cld_results <- cld(emmeans_results, Letters = letters)
print(cld_results)

## bar chart
# Install and load ggplot2 if not already installed
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Convert CLD results to a data frame (ensures it's in the correct format)
cld_df <- as.data.frame(cld_results)

# Ensure that the grouping factor is ordered correctly
cld_df$Result_Genotype <- factor(cld_df$Result_Genotype, levels = unique(cld_df$Result_Genotype))
cld_df$Result_Type <- factor(cld_df$Result_Type, levels = unique(cld_df$Result_Type))
# this groups the results in the desired manner and only selects the unique values for each treatment type and genotype

# Rename columns for clarity (optional)
colnames(cld_df)[colnames(cld_df) == ".group"] <- "CLD_Group"

# Create the bar plot
ggplot(cld_df, aes(x = Result_Type, y = emmean, fill = Result_Genotype)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                position = position_dodge(width = 0.8), width = 0.25) +
  geom_text(aes(label = CLD_Group, y = emmean + SE + 0.1),
            position = position_dodge(width = 0.8), vjust = 0) +
  labs(x = "Result Type", y = "Estimated Marginal Means", fill = "Result Genotype",
       title = "Bar Plot with Compact Letter Display") +
  theme_minimal()
