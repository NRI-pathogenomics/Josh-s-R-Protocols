#Mixed Model Test
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
if(("ggpubr" %in% all_packages)==FALSE){
  install.packages("ggpubr")}
if(("rstatix" %in% all_packages)==FALSE){
  install.packages("rstatix")}

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
library(stats)
library(rstatix)
library(ggpubr)

if(exists("Results") == FALSE){
  Results <- read.csv(file = "/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Output Files/AUDPC values.csv")
}

if(exists("anv.data") == FALSE){
  print(anv.data<-Results)
  attach(anv.data) 
}


if(exists("transformed_data") == FALSE){
  transformed_AUDPC <- bestNormalize(anv.data$Result_AUDPC)
  
  transformed_data <- transformed_AUDPC$x.t
}

options(warn=-1)

# Significant differences - this part is the hypothesis

# convert Result_Type in the anv.data to a factor

anv.data$Result_Type <- factor(anv.data$Result_Type)


# Fit the linear models

lm_ex_selected <- lm(transformed_data ~ Result_Type * Result_Genotype, data = anv.data)

# Compute estimated marginal means
l_selected <- emmeans(lm_ex_selected, list(pairwise ~ Result_Type | Result_Genotype), adjust = c("tukey"))

# need to convert to ghlt!

glht_ex_selected <- glht(lm_ex_selected, linfct = mcp(Result_Type = "Tukey"))


print(glht_ex_selected)

#We use glht() to compute the linear hypothesis tests based on the linear model lm_ex1. 
#Here, mcp(Result_Type = "Tukey") specifies that we want Tukey's method for pairwise comparisons between levels of Result_Type.

cld_ex1 <- cld(glht_ex1, Letters=letters, alpha=0.05, reversed=T)
print(cld_ex1)

# The actual non-parametric tests
#Kruskal-Wallis rank sum test
non_para_result <- kruskal.test(Result_AUDPC ~ Result_Type, data = anv.data)
base::summary(non_para_result)
print(non_para_result)

#Friedman test

# Aggregate the data to ensure no replicates
aggregated_data <- anv.data %>%
  group_by(Result_Genotype, Result_Type) %>%
  dplyr::summarize(Result_AUDPC = mean(Result_AUDPC), .groups = 'drop')

# Perform the Friedman test
friedman_result <- aggregated_data %>%
  friedman_test(Result_AUDPC ~ Result_Type | Result_Genotype)
base::summary(friedman_result)
print(friedman_result)

# Poisson distribution (general linearisation model)

Pois_model <- glm(Result_AUDPC ~ Result_Type * Result_Genotype, family = poisson(), data = anv.data)
base::summary(Pois_model)
print(Pois_model)


