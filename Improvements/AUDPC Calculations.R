#Corrected AUDPC values
all_packages <- installed.packages()

if(("dplyr" %in% all_packages)==FALSE){
  install.packages("dplyr")}
if(("agricolae" %in% all_packages)==FALSE){
  install.packages("agricolae")}
if(("agricolaeplotr" %in% all_packages)==FALSE){
  install.packages("agricolaeplotr")}
if(("tidyverse" %in% all_packages)==FALSE){
  install.packages("tidyverse")}
if(("plotrix" %in% all_packages)==FALSE){
  install.packages("plotrix")}

library(agricolae)
library(agricolaeplotr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tidyverse)
library(magrittr)
library(plotrix)

library(agricolae)
library(binhf)
library(dunn.test)
library(DescTools)
library(rcompanion)
library(FSA)
setwd("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Improvements")
pheno<-read.csv("NEWPT2.csv")

# calculate the AUDPC values - this formula calculates the AUDPC properly unlike the combined average plots
head(pheno)
days<-c(28,35,49,56) # you could use hours and put more time points
pheno[,1]=as.factor(pheno[,1]) 
pheno[,2]=as.factor(pheno[,2]) 
pheno[,3]=as.factor(pheno[,3]) 
pheno[,4]=as.factor(pheno[,4]) # you need to make all extra label/ treatment/rep/block columns in your table factors
pheno[,9]=audpc(pheno[,5:8],days) 
# pheno[,5]= pheno[,5]/ rep_number *100 # there's a function that calcs raudpc automatically 
colnames(pheno)[9]=c('audpc')
# colnames(pheno)[5]=c('audpc_r')
write.csv(pheno, file = "audpc.csv", row.names=FALSE)

Results <- read.csv("audpc.csv")


## Calculate the Mean values

## check if mock output files exist and create them if not in the Onedrive local repository
if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Combined Average plots/Mock Averaged Results")==FALSE){
  file.create("Mock Averaged Results")
}

if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Combined Average plots/Innoculated Averaged Results")==FALSE){
  file.create("Innoculated Averaged Results")
}
## Calculate and save the Averaged AUPDC results
Genotypes <- as.list(as.character(pheno$Genotype))
Genotypes <- unique(Genotypes)
GenoNumber <- length(Genotypes)
Average_AUDPC <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(Average_AUDPC) <- c('Genotype', 'Treatment', 'Average_AUDPC')
for(i in 1:GenoNumber){
  mock_audpc <- mean(pheno$audpc[pheno$Genotype == Genotypes[i] & pheno$treatment. == "M"])
  Average_AUDPC[nrow(Average_AUDPC) + 1,] = c(Genotypes[i], "M", mock_audpc)
  inno_audpc <- mean(pheno$audpc[pheno$Genotype == Genotypes[i] & pheno$treatment. == "I"])
  Average_AUDPC[nrow(Average_AUDPC) + 1,] = c(Genotypes[i], "I", inno_audpc)
}

Mean_AUDPC <- write.csv(Average_AUDPC, file = "mean_audpc", row.names = FALSE)
