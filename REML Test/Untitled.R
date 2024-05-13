##REML test
if(("fields" %in% all_packages)==FALSE){
  install.packages("fields")}

if(("spam" %in% all_packages)==FALSE){
  install.packages("spam")}

library("fields")
library("spam")

# Load the Data
if(exists("Results") == FALSE){
  Results <- read.csv(file = "/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh-R-Protocols/Output Files/AUDPC values.csv")
}

if(exists("Genotypes") == FALSE){
  Genotypes <- unique(unlist(Results$Result_Genotype))
}

if(exists("GenoNumber") == FALSE){
  GenoNumber <- length(Genotypes)
}

if(exists("Indv_Mock_Results") == FALSE){
  Indv_Mock_Results <- subset(Results, Result_Type == "M")
}

if(exists("Indv_Mock_Results") == FALSE){
  Indv_Inno_Results <- subset(Results, Result_Type == "I")
}

REML_Results <- REML.test(x, y, rho, sigma2, theta, nu = 1.5)