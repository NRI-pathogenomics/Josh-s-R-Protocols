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

# Add data to original data set
lamEx1<-cbind(anv.data, anv.data$Result_AUDPC^(-2))
lamEx2<-cbind(anv.data, anv.data$Result_AUDPC^(-1.5))
lamEx3<-cbind(anv.data, anv.data$Result_AUDPC^1)

# Create a variable with transformed values
Ex1<-(anv.data$Result_AUDPC^(-2))
Ex2<-(anv.data$Result_AUDPC^(-1.5))
Ex3<-(anv.data$Result_AUDPC^1)

# Check normality with hist, qqplots
par(mfrow=c(4,2))
hist(Ex1, main = "Distribution of AUDPC Values^-2")
qqnorm(Ex1)
qqline(Ex1)
hist(Ex2, main = "Distribution of AUDPC Values^-1.5")
qqnorm(Ex2)
qqline(Ex2)
hist(Ex3, main = "Distribution of AUDPC Values^1")
qqnorm(Ex3)
qqline(Ex3)

# Check normalitly with shapiro-wilks on all transformed data eg Ex1 Ex2 Ex3
shapiro.test(Ex1)

# Run 2-way Anova with normalised data Ex2 was the best transformation

anv.mod<-aov(Ex2 ~ Result_Type * Result_Genotype, data = anv.data)
print(anv.mod)

# Significant differences

# convert Result_Type in the anv.data to a factor

anv.data$Result_Type <- factor(anv.data$Result_Type)
# Fit the linear models

lm_ex1 <- lm(Ex1 ~ Result_Type * Result_Genotype, data = anv.data)
lm_ex2 <- lm(Ex2 ~ Result_Type * Result_Genotype, data = anv.data)
lm_ex3 <- lm(Ex3 ~ Result_Type * Result_Genotype, data = anv.data)
# Compute estimated marginal means
l1 <- emmeans(lm_ex1, list(pairwise ~ Result_Type | Result_Genotype), adjust = c("tukey"))
l2 <- emmeans(lm_ex2, list(pairwise ~ Result_Type | Result_Genotype), adjust = c("tukey"))
l3 <- emmeans(lm_ex3, list(pairwise ~ Result_Type | Result_Genotype), adjust = c("tukey"))
# need to convert to ghlt!

glht_ex1 <- glht(lm_ex1, linfct = mcp(Result_Type = "Tukey"))
glht_ex2 <- glht(lm_ex2, linfct = mcp(Result_Type = "Tukey"))
glht_ex3 <- glht(lm_ex3, linfct = mcp(Result_Type = "Tukey"))

glht_ex1
glht_ex2
glht_ex3
#We use glht() to compute the linear hypothesis tests based on the linear model lm_ex1. 
#Here, mcp(Result_Type = "Tukey") specifies that we want Tukey's method for pairwise comparisons between levels of Result_Type.

cld_ex1 <- cld(glht_ex1, Letters=letters, alpha=0.05, reversed=T)
cld_ex1
cld_ex2 <- cld(glht_ex2, Letters=letters, alpha=0.05, reversed=T)
cld_ex2
cld_ex3 <- cld(glht_ex3, Letters=letters, alpha=0.05, reversed=T)
cld_ex3

# examine differences between specific pairs of treatments, we can use a post-hoc test, 
#e.g., Tukeys Honest Significant Differences
print(posthoc<-TukeyHSD(anv.mod,"Result_Type"))