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
# mock REML
# define the y, x and v values

y <- as.numeric(Indv_Mock_Results$Result_AUDPC)

v <- rep(var(y), nrow(Indv_Mock_Results))

Result_Genotype <- unlist(Indv_Mock_Results$Result_Genotype)
Result_Type <- unlist(Indv_Mock_Results$Result_Type)

# Create x matrix
x <- cbind(Result_Genotype, Result_Type)

reml(y, v, x, data=Indv_Mock_Results, RE.constraints = NULL, RE.startvalues = 0.1)

# inno REML
# define the y, x and v values

y <- as.numeric(Indv_Inno_Results$Result_AUDPC)

v <- rep(var(y), nrow(Indv_Inno_Results))

Result_Genotype <- unlist(Indv_Inno_Results$Result_Genotype)
Result_Type <- unlist(Indv_Inno_Results$Result_Type)

# Create x matrix
x <- cbind(Result_Genotype, Result_Type)

reml(y, v, x, data=Indv_Inno_Results, RE.constraints = NULL, RE.startvalues = 0.1)

