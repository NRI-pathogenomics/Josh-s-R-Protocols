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

if(exists("Indv_Mock_Results") == FALSE){
  Indv_Mock_Results <- subset(Results, Result_Type == "M")
}

if(exists("Indv_Mock_Results") == FALSE){
  Indv_Inno_Results <- subset(Results, Result_Type == "I")
}

Mock_SW_test <- matrix(data = NA, nrow=GenoNumber, ncol=2, dimnames = NULL)
SW_Test_Results <- list()
for(i in 1:GenoNumber){
 Mockotype <- subset(Indv_Mock_Results, Result_Genotype == Genotypes[i])
 print(paste("Results for Mock Treatment of Genotype ", Genotypes[i]))
 tryCatch({
   SW_Test <- shapiro.test(as.numeric(Mockotype$Result_AUDPC))
   print(SW_Test)
 }, error = function(e) {
   if (length(unique(Mockotype$Result_AUDPC)) == 1) {
     print(paste("All Values Identical. The mock group for ", Genotypes[i], "Has a Non-Normal Distribution"))
   } else {
     write(x = c("Mock Treatment: ", Genotypes[i],SW_Test), file = paste("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Shapiro Tests/", 
                                                                         "Mock Treatment ", Genotypes[i], ".txt"))
     print("saved")
     stop(e)
   }
 })
 Innotype <- subset(Indv_Inno_Results, Result_Genotype == Genotypes[i])
 print(paste("Results for Inocculated Treatment of Genotype ", Genotypes[i]))
 tryCatch({
   SW_Test <- shapiro.test(as.numeric(Innotype$Result_AUDPC))
   print(SW_Test)
 }, error = function(e) {
   if (length(unique(Innotype$Result_AUDPC)) == 1) {
     print(paste("All Values Identical. The inocculated group for ", Genotypes[i], "Has a Non-Normal Distribution"))
   } else {
     write(x = c("Innoculated Treatment: ", Genotypes[i],SW_Test), file = paste("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Shapiro Tests/", 
                                                                         "Inoculated Treatment ", Genotypes[i], ".txt"))
     print("saved")
     stop(e)
   }
 })

}
