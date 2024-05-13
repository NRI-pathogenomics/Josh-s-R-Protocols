##REML test
if(("metaSEM" %in% all_packages)==FALSE){
  install.packages("metaSEM")}

library("metaSEM")

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

reml(y, v, x, data, RE.constraints = NULL, RE.startvalues = 0.1)
