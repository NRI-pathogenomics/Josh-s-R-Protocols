#AUDPC Helen Data
all_packages <- installed.packages()

if(("dplyr" %in% all_packages)==FALSE){
  install.packages("dplyr")}
if(("agricolae" %in% all_packages)==FALSE){
  install.packages("agricolae")}
if(("agricolaeplotr" %in% all_packages)==FALSE){
  install.packages("agricolaeplotr")}

library(agricolae)
library(agricolaeplotr)
library(dplyr)
library(ggplot2)
Path_Assay <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/GitHub-Josh-s-R-Protocols/Input Files/NEWPT2.csv", 
                       header = TRUE, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE, comment.char = "")
Path_Assay <- setNames(Path_Assay, nm=c("Genotype", "block",	"treatment.", "number", "Day 28", "Day 35", "Day 49", "Day 56"))


if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/GitHub-Josh-s-R-Protocols/Combined Average plots/Mock Averaged Results")==FALSE){
  file.create("Mock Averaged Results")
}

if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/GitHub-Josh-s-R-Protocols/Combined Average plots/Innoculated Averaged Results")==FALSE){
  file.create("Innoculated Averaged Results")
}
##Subsetting the Data
Inno_Sub <- subset(Path_Assay, treatment.=="I")
Mock_Sub <- subset(Path_Assay, treatment.=="M")
##Individual AUDPC
Tail_Value <- Path_Assay$block[length(Path_Assay$block)]

setNames(Path_Assay, nm=c("Genotype", "block",	"treatment.", "number", "Day 28", "Day 35", "Day 49", "Day 56"))

Result_Genotype <- list()
Result_Treatment <- list()
Result_AUDPC <-list()
Result_Type <- list()
for(i in 1:Tail_Value){
  Treatment_Block <- as.numeric(i)
  Treatment_value <- paste0("Treatment Block: ", i)
  print(Treatment_value)
  Block <- subset(Path_Assay, block==i)
  # print(evaluation)
  # Next step is to plot multiple lines on the same graph by seperating them by Genotype and Treatment 
  # (using another for loop selecting the column in evalutation using evaluation$column)
  # print("Mock Group: ")
  Mock <- subset(Block, treatment.=="M")
  Genotypes <- c(unique(Mock$Genotype))
  GenoNumber <- length(Genotypes)
  MockAUDPC <- list()
  for(i in 1:GenoNumber){
    Mockotype <- subset(Mock, Genotype==Genotypes[i])
    #Calculate audpc using agricolae on the subset then leave room for the plots, same for innoculated
    dates <- c(28,35,49,56)
    evaluation <- unlist(Mockotype[5:8], use.names=FALSE)
    AUDPC <- audpc(evaluation, dates, type="absolute")
    #print(paste0("AUDPC for ",Genotypes[i], " is: ", AUDPC))
    Result_Genotype <- append(Result_Genotype, Genotypes[i])
    Result_Treatment <- append(Result_Treatment,Treatment_Block)
    Result_AUDPC <-append(Result_AUDPC,AUDPC)
    Result_Type <- append(Result_Type, Mockotype$treatment.)
    #Insert ggplot2 code here
  }
  Innoculated <- subset(Block, treatment.=="I")
  Genotypes <- c(unique(Innoculated$Genotype))
  GenoNumber <- length(Genotypes)
  # print("Innoculated Group: ")
  for(i in 1:GenoNumber){
    Innotype <- subset(Innoculated, Genotype==Genotypes[i])
    #Calculate audpc using agricolae on the subset then leave room for the plots, same for innoculated
    dates <- c(28,35,49,56)
    evaluation <- unlist(Mockotype[5:8], use.names = FALSE)
    AUDPC <- audpc(evaluation, dates, type="absolute")
    #print(paste0("AUDPC for ",Genotypes[i], " is: ", AUDPC))
    Result_Genotype <- append(Result_Genotype, Genotypes[i])
    Result_Treatment <- append(Result_Treatment,Treatment_Block)
    Result_AUDPC <-append(Result_AUDPC,AUDPC)
    Result_Type <- append(Result_Type, Innotype$treatment.)
  }
  
}
#Generate the Results and reform the original
Results <- cbind(Result_Genotype,Result_Type,Result_Treatment, Result_AUDPC)
Indv_Mock_Results <- subset(Results, Result_Type=="M")
Indv_Inno_Results <- subset(Results, Result_Type=="I")

# FORGET THE AVERAGE VALUES AND PLOTTING THEM FOR NOW KEEP THE PROGRAMS SEPERATE! (COME BACK AT A LATER )
##Plot Data Here
#Insert AUDPC plot for Mock and Innoculated here

##Mock AUDPC plot
# Reshape the data from wide to long format - It is NOT Going to work plotting the individual data on the same plot like this 
# subset the different genotypes and plot them on the same plots but keep the genotypes seperate or go back to using averaged values and plot them on the same plots
Mock_long <- tidyr::pivot_longer(Mock_Sub, cols = starts_with("Day"), names_to = "Day", values_to = "Disease_Score")

# Convert Day column to numeric
Mock_long$Day <- as.numeric(gsub("Day ", "", Mock_long$Day))

# Create plot using ggplot2
ggplot(Mock_long, aes(x = Day, y = Disease_Score, group = interaction(Genotype, block, treatment., number), color = interaction(Genotype, block, treatment., number))) +
  geom_line() +
  labs(x = "Day", y = "Disease Score", color = "Group") +
  theme_minimal()

##Inno AUDPC plot


#Insert Historgram of AUDPC and Standard Deviation

