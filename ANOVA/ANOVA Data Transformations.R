#ANOVA Data Transformations

# Load Package MASS
# attach package library(MASS)
# Load and attach data
all_packages <- installed.packages()

if(("MASS" %in% all_packages)==FALSE){
  install.packages("MASS")}
if(("ggpmisc" %in% all_packages)==FALSE){
  install.packages("ggpmisc")}
if(("jmv" %in% all_packages)==FALSE){
  install.packages("jmv")}
if(("Rmisc" %in% all_packages)==FALSE){
  install.packages("Rmisc")}
if(("emmeans" %in% all_packages)==FALSE){
  install.packages("emmeans")}
if(("multcomp" %in% all_packages)==FALSE){
  install.packages("multcomp")}
if(("Hmisc" %in% all_packages)==FALSE){
  install.packages("Hmisc")}
if(("lattice" %in% all_packages)==FALSE){
  install.packages("lattice")}
if(("multcompView" %in% all_packages)==FALSE){
  install.packages("multcompView")}
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
# Load the package
## Power Of - Transformations

# Create empty lists to store models and transformed data
lm_models <- list()
Ex_values <- list()

# Loop through the powers from -2 to 2 going up by 0.1 each time
for (power in seq(-2, 2, by = 0.1)) {
  # Try to transform data with the current power
  tryCatch({
    # Add data to original data set
    lamEx <- cbind(anv.data, anv.data$Result_AUDPC^power)
    Ex <- anv.data$Result_AUDPC^power
    
    # Append lm model and transformed data to lists
    lm_models[[as.character(power)]] <- lm(Ex ~ Result_Type * Result_Genotype, data = lamEx)
    Ex_values[[as.character(power)]] <- Ex
    
    # Plot histogram
    hist(Ex, main = paste("Distribution of AUDPC Values^", power))
    qqnorm(Ex)
    qqline(Ex)
    
    # Check normality with Shapiro-Wilk test
    shapiro.test(Ex)
  }, error = function(e) {
    # If an error occurs (e.g., data is transformed by the power of 0), print a message
    cat("Error occurred for power", power, ": ", conditionMessage(e), "\n")
  })
}

# BestNormalize transformations
# Best Normalize

transformed_AUDPC <- bestNormalize(anv.data$Result_AUDPC)

transformed_data <- transformed_AUDPC$x.t

hist(transformed_data)
qqnorm(transformed_data)
qqline(transformed_data)
shapiro.test(transformed_data)

# save all Models and transformed data
lm_Ex_all <- lm_models
Ex_all <- Ex_values

# #redo ANOVA test by selecting a set of selected data in EX1
# anv.mod<-aov(Ex_all[["1"]] ~ Result_Type * Result_Genotype, data = anv.data)
# print(anv.mod)

# Significant differences

# convert Result_Type in the anv.data to a factor

anv.data$Result_Type <- factor(anv.data$Result_Type)
# Fit the linear models

# lm_ex_selected <- lm(Ex_[["1"]] ~ Result_Type * Result_Genotype, data = anv.data)
lm_ex_selected <- lm(transformed_data ~ Result_Type * Result_Genotype, data = anv.data)

# Compute estimated marginal means
l_selected <- emmeans(lm_ex_selected, list(pairwise ~ Result_Type | Result_Genotype), adjust = c("tukey"))

# need to convert to ghlt!

glht_ex_selected <- glht(lm_ex_selected, linfct = mcp(Result_Type = "Tukey"))


glht_ex_selected

#We use glht() to compute the linear hypothesis tests based on the linear model lm_ex1. 
#Here, mcp(Result_Type = "Tukey") specifies that we want Tukey's method for pairwise comparisons between levels of Result_Type.

cld_ex1 <- cld(glht_ex1, Letters=letters, alpha=0.05, reversed=T)
cld_ex1

#remake the anv model usig the transformed data

# Run anv.mod for the transformed data
anv.mod <- aov(transformed_data ~ Result_Type * Result_Genotype, data = anv.data)

# examine differences between specific pairs of treatments, we can use a post-hoc test, 
#e.g., Tukeys Honest Significant Differences
print(posthoc<-TukeyHSD(anv.mod,"Result_Type"))
