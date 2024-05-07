# Load Package MASS
# attach package library(MASS)
# Load and attach data
all_packages <- installed.packages()

if(("MASS" %in% all_packages)==FALSE){
  install.packages("MASS")}
library(MASS)
library(tidyverse)

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

# examine differences between specific pairs of treatments, we can use a post-hoc test, 
#e.g., Tukeys Honest Significant Differences
 print(posthoc<-TukeyHSD(anv.mod,"Result_Type"))
 