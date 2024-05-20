# Master Script
# AUDPC
source("~/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Combined Average Plots/Combined Average plots.R")

#Shapiro tests

#source("~/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Shapiro Tests/Shapiro Wilks Test.R")

# ANOVA tests
source("~/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/ANOVA/Anova example.R")

question_1 <- readline("Do you want to try transform the data?")

if(question_1 == "Y"){
  source("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/ANOVA/ANOVA Data Transformations.R")
}
if(question_1 == "N"){
  # REML test
  
  source("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/REML Test.R") 
}



# Residuals test

source("~/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Shapiro Tests/Residuals Test.R")

