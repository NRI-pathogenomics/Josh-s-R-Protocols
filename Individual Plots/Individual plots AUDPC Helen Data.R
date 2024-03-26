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
Path_Assay <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Master's/Research/Bioinformatic work/iDEP trainin/Helen Data/NEWPT2.csv", 
                       header = TRUE, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE, comment.char = "")
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
  plot_data <- as.data.frame(cbind(dates, evaluation))
  plot <- ggplot(data=plot_data, mapping=aes(x=dates, y=evaluation, )) + 
    geom_line() + geom_point() + 
    annotate("text", x = 40, y = 1.5, label = paste0("AUDPC: ", as.character(AUDPC))) +
    geom_ribbon(aes(ymin = min(evaluation), ymax = evaluation), fill = "#1b98e0")
  plot <- plot + labs(title = paste0(Mockotype$Genotype, " Mock Innoculation ", Treatment_value))
  show(plot)
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
    #Insert ggplot2 code here
    plot_data <- as.data.frame(cbind(dates, evaluation))
    plot <- ggplot(data=plot_data, mapping=aes(x=dates, y=evaluation, )) + 
      geom_line() + 
      geom_point() + 
      annotate("text", x = 40, y = 1.5, label = paste0("AUDPC: ", as.character(AUDPC))) +
      geom_ribbon(aes(ymin = min(evaluation), ymax = evaluation), fill = "#1b98e0")
    plot <- plot + labs(title = paste0(Innotype$Genotype, " Verticilium Innoculation ", Treatment_value))
    show(plot)
  }
  
}

Results <- cbind(Result_Genotype,Result_Type,Result_Treatment, Result_AUDPC)
show(Results)
write.csv(Results, file = "/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Master's/Research/Bioinformatic work/iDEP trainin/Helen Data/AUDPC Values.csv")
# Save all plots
plots.dir.path <- list.files(tempdir(), pattern = "rs-graphics", full.names = TRUE)
plots.png.paths <- list.files(plots.dir.path, pattern = ".png", full.names = TRUE)
file.copy(from = plots.png.paths,
          to = "/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Master's/Research/Bioinformatic work/iDEP trainin/Helen Data/Individual Plots",
          overwrite = TRUE)

