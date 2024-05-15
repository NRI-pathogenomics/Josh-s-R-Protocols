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

print(anv.data<-Results)
attach(anv.data)

anv.data$Result_AUDPC <- as.numeric(anv.data$Result_AUDPC)
anv.data$Result_Genotype <- as.character(Result_Genotype)
anv.data$Result_Type <- as.character(Result_Type)
# Create a variable for boxcox
# Run aov
anv.model <- aov(Result_AUDPC ~ Result_Type * Result_Genotype, data = anv.data)

# here x would be disease score, y would be genotype z could be included as block or run without for a one way anova

print(anv.model)

# Check normality with hist, qqplots
 par(mfrow=c(4,2))
 hist(anv.data$Result_AUDPC)
 qqnorm(anv.data$Result_AUDPC)
 qqline(anv.data$Result_AUDPC)
# Check normalitly with shapiro-wilks
 shapiro.test(anv.data$Result_AUDPC)

# If data isnt normal Run a Box-Cox proceedure to obtain optimal transformation
 boxcox(anv.model)
# Produces a plot of likelihood of the parameter lambda against values of lambda
# from -2 to 2
# Dotted vertical lines indicate the ideal value of lambda
# Refine range of lambda eg from 0 to 0.5 in increments of 0.1
 boxcox(anv.model, lambda = seq(0, 0.5, 0.1))

# Plot boxcoxs
 par(mfrow=c(2,1))
 boxcox(anv.model)
 boxcox(anv.model, lambda = seq(0, 0.5, 0.1))

# Add data to original data set
 lamEx1<-cbind(anv.data, anv.data$Result_AUDPC^0.17)
 lamEx2<-cbind(anv.data, anv.data$Result_AUDPC^0.26)
 lamEx3<-cbind(anv.data, anv.data$Result_AUDPC^0.35)

# Create a variable with transformed values
 Ex1<-(anv.data$Result_AUDPC^0.17)
 Ex2<-(anv.data$Result_AUDPC^0.26)
 Ex3<-(anv.data$Result_AUDPC^0.35)

# Check normality with hist, qqplots
 par(mfrow=c(4,2))
 hist(Ex1)
 qqnorm(Ex1)
 qqline(Ex1)
 hist(Ex2)
 qqnorm(Ex2)
 qqline(Ex2)
 hist(Ex3)
 qqnorm(Ex3)
 qqline(Ex3)

# Check normalitly with shapiro-wilks on all transformed data eg Ex1 Ex2 Ex3
 shapiro.test(Ex1)

# Run 2-way Anova with normalised data Ex2 was the best transformation
 anv.mod<-aov(Ex2 ~ Result_Type * Result_Genotype, data = anv.data)
 print(anv.mod)

# Significant differences

# Fit the linear model
lm_ex1 <- lm(Ex1 ~ Result_Type * Result_Genotype, data = anv.data)

# Compute estimated marginal means
l1 <- emmeans(lm_ex1, list(pairwise ~ Result_Type | Result_Genotype), adjust = c("tukey"))

# Extract the variance-covariance matrix from the model object
V1 <- vcov(lm_ex1)

# Coerce the object `l1` to class "emmGrid"
l1 <- as.emmGrid(l1)

#set the variance level, by setting the "var" attribute of L1 to V1
attr(l1, "var") <- V1
# Assign the variance-covariance matrix to the `var` slot

cld(l1, Letters=letters, alpha=0.05, reversed=T)

lm_ex2 <- lm(Ex2 ~ Result_Genotype * Result_Type, data = anv.data)
summary(lm_ex2)
l2 <- emmeans(lm_ex2, list(pairwise ~ Result_Type | Result_Genotype ), adjust =c("tukey"))
# Extract the variance-covariance matrix from the model object
V2 <- vcov(lm_ex2)

# Create an "emmGrid" object
l2_emmgrid <- emmGrid(lattice = l2, var = V2)

cld(l2_emmgrid, Letters=letters, alpha=0.05, reversed=T)

lm_ex3 <- lm(Ex3 ~ Result_Type * Result_Genotype, data = anv.data)
l3 <- emmeans(lm_ex3, list(pairwise ~ Result_Type | Result_Genotype ), adjust =c("tukey"))
# Extract the variance-covariance matrix from the model object
V3 <- vcov(lm_ex3)

# Create an "emmGrid" object
l3_emmgrid <- emmGrid(lattice = l3, var = V3)

cld(l3_emmgrid, Letters=letters, alpha=0.05, reversed=T)
 
# examine differences between specific pairs of treatments, we can use a post-hoc test, 
#e.g., Tukeys Honest Significant Differences
 print(posthoc<-TukeyHSD(anv.mod,"Result_Type"))
 