# Master Script
# AUDPC
source("~/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Combined Average Plots/Combined Average plots.R")


# ANOVA tests
source("~/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/ANOVA/Anova example.R")


User_Input <- readline("Do you want to transform the data using the BestNormalize function? Y/N")

if(User_Input == "Y"){
  source("~/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/ANOVA/ANOVA With Transformation.R")
  User_Input2 <- readline("Do you want to run non-parametric tests? Y/N")
  if(User_Input2 == "Y"){
    source("~/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Mixed Models Test/glht linear model test.R")
    }
  if(User_Input2 == "N"){
    exit()
  }
}

if(User_Input == "N"){
  source("~/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Mixed Models Test/glht linear model test.R")
}