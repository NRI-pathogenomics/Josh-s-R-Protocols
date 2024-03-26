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
Path_Assay <- setNames(Path_Assay, nm=c("Genotype", "block",	"treatment.", "number", "Day 28", "Day 35", "Day 49", "Day 56"))

Genotypes <- c(unique(Path_Assay$Genotype))
GenoNumber <- length(Genotypes)

Mock <- subset(Path_Assay, treatment.=="M")
Mock_means <- data.frame()
Mock_AUDPC <- list()
if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/Master's
               /Research/Bioinformatic work/iDEP trainin/Helen Data/Mock Averaged Results")==FALSE){
  file.create("Mock Averaged Results")
}

if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/Master's
               /Research/Bioinformatic work/iDEP trainin/Helen Data/Innoculated Averaged Results")==FALSE){
  file.create("Innoculated Averaged Results")
}

Innoculated <- subset(Path_Assay, treatment.=="I")
Inno_means <- data.frame
Inno_AUDPC <- list()
# print("Innoculated Group: ")
#Mock Means
for(i in 1:GenoNumber){
  Mockotype <- subset(Mock, Genotype==Genotypes[i])
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

  #Insert ggplot2 code here
plot_data <- as.data.frame(cbind(dates, means))
plot <- ggplot(data=plot_data, mapping=aes(x=dates, y=means, )) + geom_line() + geom_point() + 
  annotate("text", x = 35, y = 3.5, label = paste0("AUDPC: ", as.character(AUDPC))) +
  geom_ribbon(aes(ymin = min(means), ymax = means), fill = "#1b98e0")
plot <- plot + labs(title = paste0(Genotypes[i], " Mock Innoculation Average Disease Progression"))
show(plot)
}


# Innoculated means
for(i in 1:GenoNumber){
  Innotype <- subset(Innoculated, Genotype==Genotypes[i])
  #Calculate audpc using agricolae on the subset then leave room for the plots, same for innoculated
  print(paste0("Averages for ", Genotypes[i]))
  dates <- c(28,35,49,56)
  means <- colMeans(Innotype[5:8])
  print(means)
  AUDPC <- audpc(means, dates, type="absolute")
  print(paste0("AUDPC for ",Genotypes[i], " is: ", AUDPC))
  Inno_AUDPC <-append(Inno_AUDPC,AUDPC)
  #Insert ggplot2 code here
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
Save_File_Mock <- "/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Master's/Research/Bioinformatic work/iDEP trainin/Helen Data/Averaged Plots/MockAUDPC.csv"
Save_File_Inno <- "/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Master's/Research/Bioinformatic work/iDEP trainin/Helen Data/Averaged Plots/InnoAUDPC.csv"
if(file.exists(Save_File_Mock) == TRUE){
  file.remove(Save_File_Mock)
}
if(file.exists(Save_File_Inno) == TRUE){
  file.remove(Save_File_Inno)
}
write.csv(x = Mock_results, file = Save_File_Mock )
write.csv(x = Inno_results, file = Save_File_Inno)

#Save Plots
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Master's/Research/Bioinformatic work/iDEP trainin/Helen Data/Averaged Plots")

##Extra Plot the Average plots all on the same plot
#Mock Means
# Reshape the data from wide to long format
Mock_long <- tidyr::pivot_longer(Mock_Sub, cols = starts_with("Day"), names_to = "Day", values_to = "Disease_Score")

# Convert Day column to numeric
Mock_long$Day <- as.numeric(gsub("Day ", "", Mock_long$Day))

# Create plot using ggplot2
ggplot(Mock_long, aes(x = Day, y = Disease_Score, group = interaction(Genotype, block, treatment., number), color = interaction(Genotype, block, treatment., number))) +
  geom_line() +
  labs(x = "Day", y = "Disease Score", color = "Group") +
  theme_minimal()
