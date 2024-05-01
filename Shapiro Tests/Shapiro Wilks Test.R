#Shapiro-Wilkes Test
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

if(exists(Indv_Mock_Results) == FALSE){
  Indv_Mock_Results <- subset(Results, Result_Type == "M")
}

if(exists(Indv_Mock_Results) == FALSE){
  Indv_Inno_Results <- subset(Results, Result_Type == "I")
}

Mock_SW_test <- matrix(data = NA, nrow=GenoNumber, ncol=2, dimnames = c("Genotype", "Normal Distribution?"))

for(i in 1:GenoNumber){
 Mockotype <- subset(Indv_Mock_Results, Result_Genotype == Genotypes[i])
 SW_Test <- shapiro.test(Mockotype)
 Mock_SW_test <- append(Mock_SW_test, cbind(Genotypes[i], SW_test))
}
