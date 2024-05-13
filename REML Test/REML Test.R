##REML test
# if(("metaSEM" %in% all_packages)==FALSE){
#   install.packages("metaSEM")}

library("metaSEM")
library("dplyr")

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
library(tidyr)
Indv_Mock_Results$Result_Type <- as.character(Indv_Mock_Results$Result_Type)
Indv_Mock_Results$Result_Genotype <- as.character(Indv_Mock_Results$Result_Genotype)

# Pivot the data wider
reshaped_data <- Indv_Mock_Results %>%
  pivot_wider(
    id_cols = c(Result_Type, Result_Genotype),
    names_from = Result_Treatment,
    values_from = Result_AUDPC
  )

# Extract necessary variables
y <- Mock_column$AUDPC
v <- rep(var(y), nrow(reshaped_data))  # Variance vector (assuming homogeneous variance)
x <- NULL  # If you're not including any predictors

# Run reml() function
reml_result <- reml(y = y, v = v, x = x, data = NULL, RE.constraints = NULL, RE.startvalues = 0.1)


