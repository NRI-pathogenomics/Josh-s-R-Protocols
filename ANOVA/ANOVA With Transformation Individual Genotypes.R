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
options(warn=-1)
# Load the package
## Power Of - Transformations

# Create empty lists to store models and transformed data
lm_models <- list()
Ex_values <- list()

# # Loop through the powers from -2 to 2 going up by 0.1 each time
# for (power in seq(-2, 2, by = 0.1)) {
#   # Try to transform data with the current power
#   tryCatch({
#     # Add data to original data set
#     lamEx <- cbind(anv.data, anv.data$Result_AUDPC^power)
#     Ex <- anv.data$Result_AUDPC^power
#     
#     # Append lm model and transformed data to lists
#     lm_models[[as.character(power)]] <- lm(Ex ~ Result_Type * Result_Genotype, data = lamEx)
#     Ex_values[[as.character(power)]] <- Ex
#     
#     # Plot histogram
#     hist(Ex, main = paste("Distribution of AUDPC Values^", power))
#     qqnorm(Ex)
#     qqline(Ex)
#     
#     # Check normality with Shapiro-Wilk test
#     shapiro.test(Ex)
#   }, error = function(e) {
#     # If an error occurs (e.g., data is transformed by the power of 0), print a message
#     cat("Error occurred for power", power, ": ", conditionMessage(e), "\n")
#   })
# }

# BestNormalize transformations
# Best Normalize
Results$Result_AUDPC <- as.numeric(Results$Result_AUDPC)
Results$Result_Genotype <- as.character(Results$Result_Genotype)
Results$Result_Type <- as.character(Results$Result_Type)
transformed_AUDPC <- bestNormalize(Results$Result_AUDPC)

transformed_data <- transformed_AUDPC$x.t

Transformed_Results <- cbind(Results, transformed_data)


# #redo ANOVA test by selecting a set of selected data in EX1
# anv.mod<-aov(Ex_all[["1"]] ~ Result_Type * Result_Genotype, data = anv.data)
# Run anv.mod for the transformed data

for(i in 1:GenoNumber){
  print(paste("Running Results for Genotype: ", Genotypes[i]))
  Results_Sub <- subset(Transformed_Results, Result_Genotype == Genotypes[i])
  print(anv.data<-Results_Sub)
  attach(anv.data)
  
  anv.data$Result_AUDPC <- as.numeric(anv.data$Result_AUDPC)
  anv.data$Result_Genotype <- as.character(anv.data$Result_Genotype)
  anv.data$Result_Type <- as.character(anv.data$Result_Type)
  # Create a variable for boxcox
  # Run aov
  anv.model <- aov(transformed_data ~ Result_Type, data = anv.data)
  
  # here x would be disease score, y would be genotype z could be included as block or run without for a one way anova
  
  print(anv.model)
  
  # Check normality with hist, qqplots
  par(mfrow=c(4,2))
  hist(anv.data$Result_AUDPC, main = paste("Distribution of AUDPC values for ", Genotypes[i]))
  qqnorm(anv.data$Result_AUDPC)
  qqline(anv.data$Result_AUDPC)
  # Check normalitly with shapiro-wilks - with a tryCatch statement to catch whenever the AUDPCs are all the same
  shapiro_result <- tryCatch({
    shapiro.test(anv.data$Result_AUDPC)
  }, error = function(e) {
    message("Error in Shapiro-Wilk test for Genotype ", Genotypes[i], ": ", e$message)
    return(NULL)
  })
  
  if (is.null(shapiro_result)) {
    detach(anv.data)
    next
  }
  
  # # If data isnt normal Run a Box-Cox proceedure to obtain optimal transformation
  # MASS::boxcox(anv.model)
  # # Produces a plot of likelihood of the parameter lambda against values of lambda
  # # from -2 to 2
  # # Dotted vertical lines indicate the ideal value of lambda
  # # Refine range of lambda eg from 0 to 0.5 in increments of 0.1
  # MASS::boxcox(anv.model, lambda = seq(0, 0.5, 0.1))
  # 
  # # Plot boxcoxs
  # par(mfrow=c(2,1))
  # MASS::boxcox(anv.model)
  # MASS::boxcox(anv.model, lambda = seq(0, 0.5, 0.1))
  
  #Test the residuals
  tryCatch({
    x <- resid(lm(Result_AUDPC ~ Result_Type, data = anv.data))
    
    if (all(x == x[1])) {
      stop("all 'x' values are identical")
    }
    
    x<-resid(lm(transformed_data ~ Result_Type, data=anv.data))
    hist(x, main = "Residual Distribution of BestNormalized results for AUDPC by Type and Genotype")
    shapiro.test(x)
    m <- mean(x)
    s <- sd(x)
    ks.test(x,"pnorm",m,s)
  }, error = function(e) {
    message("Error in residuals test for Genotype ", Genotypes[i], ": ", e$message)
    detach(anv.data)
    next
  })
  
  detach(anv.data)
  # examine differences between specific pairs of treatments, we can use a post-hoc test, 
  #e.g., Tukeys Honest Significant Differences
  print(posthoc<-TukeyHSD(anv.mod,"Result_Type"))
}
