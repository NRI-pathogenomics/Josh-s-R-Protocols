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
library(reshape2)

print(anv.data<-Results)
attach(anv.data)

anv.data$Result_AUDPC <- as.numeric(anv.data$Result_AUDPC)
anv.data$Result_Genotype <- as.character(Result_Genotype)
anv.data$Result_Type <- as.character(Result_Type)
#create comb variable combining both datasets

comb <- paste(anv.data$Result_Genotype, anv.data$Result_Type, sep = "-")

anv.data$comb <- comb

anv.data <- subset(anv.data, select = c("comb", "Result_Treatment", "Result_AUDPC"))

# Create a variable for boxcox
# Run aov
anv.model <- aov(Result_AUDPC ~ comb, data = anv.data)

# here x would be disease score, y would be genotype z could be included as block or run without for a one way anova

print(anv.model)

# Check normality with hist, qqplots
 par(mfrow=c(4,2))
 hist(anv.data$Result_AUDPC, main = "Distribution of AUDPC values for Each Genotype")
 qqnorm(anv.data$Result_AUDPC)
 qqline(anv.data$Result_AUDPC)
# Check normalitly with shapiro-wilks
 shapiro.test(anv.data$Result_AUDPC)

# If data isnt normal Run a Box-Cox proceedure to obtain optimal transformation
MASS::boxcox(anv.model)
# Produces a plot of likelihood of the parameter lambda against values of lambda
# from -2 to 2
# Dotted vertical lines indicate the ideal value of lambda
# Refine range of lambda eg from 0 to 0.5 in increments of 0.1
MASS::boxcox(anv.model, lambda = seq(0, 0.5, 0.1))

# Plot boxcoxs
 par(mfrow=c(2,1))
 MASS::boxcox(anv.model)
 MASS::boxcox(anv.model, lambda = seq(0, 0.5, 0.1))
 
# Residuals Test
x<-resid(lm(Result_AUDPC ~ comb, data = anv.data))
hist(x, main = "Residual Distribution of Regression Model for AUDPC by Type and Genotype")
shapiro.test(x)
m <- mean(x)
s <- sd(x)
ks.test(x,"pnorm",m,s)
 