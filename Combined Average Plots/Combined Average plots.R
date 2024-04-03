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

## Individual AUDPCs
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
#Generate the Results and reform the original data frame
Results <- cbind(Result_Genotype,Result_Type,Result_Treatment, Result_AUDPC)
Indv_Mock_Results <- subset(Results, Result_Type=="M")
Indv_Inno_Results <- subset(Results, Result_Type=="I")

## Calculate the Mean values
# reuse Mock_sub and Inno_sub
Mock_means <- data.frame()
Mock_AUDPC <- list()
Inno_means <- data.frame()
Inno_AUDPC <- list()
## check if mock output files exist and create them if not in the Onedrive local repository
if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/GitHub-Josh-s-R-Protocols/Combined Average plots/Mock Averaged Results")==FALSE){
  file.create("Mock Averaged Results")
}

if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/GitHub-Josh-s-R-Protocols/Combined Average plots/Innoculated Averaged Results")==FALSE){
  file.create("Innoculated Averaged Results")
}
## Calculate and save the Averaged AUPDC results
Genotypes <- c(unique(Path_Assay$Genotype))
GenoNumber <- length(Genotypes)
for(i in 1:GenoNumber){
  Mockotype <- subset(Mock_Sub, Genotype==Genotypes[i])
  #Calculate audpc using agricolae on the subset then leave room for the plots, same for innoculated
  print(paste0("Averages for ", Genotypes[i]))
  dates <- c(28,35,49,56)
  means <- colMeans(Mockotype[5:8])
  print(means)
  Mock_means <- append(Mock_means, cbind(Genotypes[i], means))
  ##get it to add the means as it goes to the means dataframe
  AUDPC <- audpc(means, dates, type="absolute")
  print(paste0("AUDPC for ",Genotypes[i], " is: ", AUDPC))
  Mock_AUDPC <-append(Mock_AUDPC,AUDPC)
  
  #PLOT AS THE LOOP RUNS - TEMPORARY
  plot_data <- as.data.frame(cbind(dates, means))
  plot <- ggplot(data=plot_data, mapping=aes(x=dates, y=means, )) + geom_line() + geom_point() + 
    annotate("text", x = 35, y = 3.5, label = paste0("AUDPC: ", as.character(AUDPC))) +
    geom_ribbon(aes(ymin = min(means), ymax = means), fill = "#1b98e0")
  plot <- plot + labs(title = paste0(Genotypes[i], " Mock Innoculation Average Disease Progression"))
  show(plot)
}


# Innoculated means
for(i in 1:GenoNumber){
  Innotype <- subset(Inno_Sub, Genotype==Genotypes[i])
  #Calculate audpc using agricolae on the subset then leave room for the plots, same for innoculated
  print(paste0("Averages for ", Genotypes[i]))
  dates <- c(28,35,49,56)
  means <- colMeans(Innotype[5:8])
  print(means)
  AUDPC <- audpc(means, dates, type="absolute")
  print(paste0("AUDPC for ",Genotypes[i], " is: ", AUDPC))
  Inno_AUDPC <-append(Inno_AUDPC,AUDPC)
  #PLOT AS THE LOOP RUNS - TEMPORARY
  plot_data <- as.data.frame(cbind(dates, means))
  plot <- ggplot(data=plot_data, mapping=aes(x=dates, y=means, )) + geom_line() + geom_point() + 
    annotate("text", x = 35, y = 3.5, label = paste0("AUDPC: ", as.character(AUDPC))) +
    geom_ribbon(aes(ymin = min(means), ymax = means), fill = "#1b98e0")
  plot <- plot + labs(title = paste0(Genotypes[i], " Verticilium Innoculation Average Disease Progression"))
  show(plot)
}
#Create results data frames
Mock_results <- cbind(Genotypes, Mock_AUDPC)
Inno_results <- cbind(Genotypes, Inno_AUDPC)
#Save Results
Save_File_Mock <- "/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/GitHub-Josh-s-R-Protocols/Combined Average Plots/MockAUDPC.csv"
Save_File_Inno <- "/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/GitHub-Josh-s-R-Protocols/Combined Average Plots/InnoAUDPC.csv"
if(file.exists(Save_File_Mock) == TRUE){
  file.remove(Save_File_Mock)
}
if(file.exists(Save_File_Inno) == TRUE){
  file.remove(Save_File_Inno)
}
write.csv(x = Mock_results, file = Save_File_Mock )
write.csv(x = Inno_results, file = Save_File_Inno)

##Plot Data Here
#Insert AUDPC plot for Mock and Innoculated here

# # Mock AUDPC plot
# # Reshape the data from wide to long format - It is NOT Going to work plotting the individual data on the same plot like this 
# # subset the different genotypes and plot them on the same plots but keep the genotypes seperate or go back to using averaged values and plot them on the same plots
# Mock_long <- tidyr::pivot_longer(Mock_Sub, cols = starts_with("Day"), names_to = "Day", values_to = "Disease_Score")
# 
# # Convert Day column to numeric
# Mock_long$Day <- as.numeric(gsub("Day ", "", Mock_long$Day))
# 
# # Create plot using ggplot2
# ggplot(Mock_long, aes(x = Day, y = Disease_Score, group = interaction(Genotype, block, treatment., number), color = interaction(Genotype, block, treatment., number))) +
#   geom_line() +
#   labs(x = "Day", y = "Disease Score", color = "Group") +
#   theme_minimal()

##Inno AUDPC plot


#Insert Historgram of AUDPC and Standard Deviation

