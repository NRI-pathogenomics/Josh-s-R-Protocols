#AUDPC Helen Data
all_packages <- installed.packages()

if(("dplyr" %in% all_packages)==FALSE){
  install.packages("dplyr")}
if(("agricolae" %in% all_packages)==FALSE){
  install.packages("agricolae")}
if(("agricolaeplotr" %in% all_packages)==FALSE){
  install.packages("agricolaeplotr")}
if(("tidyverse" %in% all_packages)==FALSE){
  install.packages("tidyverse")}

library(agricolae)
library(agricolaeplotr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tidyverse)
library(magrittr)
Path_Assay <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Input Files/NEWPT2.csv", 
                       header = TRUE, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE, comment.char = "")
Path_Assay <- setNames(Path_Assay, nm=c("Genotype", "block",	"treatment.", "number", "Day 28", "Day 35", "Day 49", "Day 56"))


if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Combined Average plots/Mock Averaged Results")==FALSE){
  file.create("Mock Averaged Results")
}

if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Combined Average plots/Innoculated Averaged Results")==FALSE){
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
Results <- as.data.frame(Results, row.names = NULL)
#remove the rownames
Indv_Mock_Results <- as.data.frame(subset(Results, Result_Type=="M"))
Indv_Inno_Results <- as.data.frame(subset(Results, Result_Type=="I"))
## Calculate the Mean values
# reuse Mock_sub and Inno_sub
Mock_means <- data.frame()
Mock_AUDPC <- list()
Inno_means <- data.frame()
Inno_AUDPC <- list()
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
Genotypes <- c(unique(Path_Assay$Genotype))
GenoNumber <- length(Genotypes)
Mock_Average_Path_Scores <- matrix(data = NA, nrow=1, ncol=6, dimnames = NULL)
for(i in 1:GenoNumber){
  New_Row <- data.frame()
  Mockotype <- subset(Mock_Sub, Genotype==Genotypes[i])
  #Calculate audpc using agricolae on the subset then leave room for the plots, same for innoculated
  print(paste0("Calculating averages for Mock ", Genotypes[i]))
  dates <- c(28,35,49,56)
  means <- colMeans(Mockotype[5:8])
  New_Row <- means
  New_Row$Genotype <- Genotypes[i]
  New_Row$Treatment <- c(unique(Mockotype$treatment.))
  Mock_Average_Path_Scores <- rbind(Mock_Average_Path_Scores, New_Row, deparse.level = 0)
  ##get it to add the means as it goes to the means dataframe
  AUDPC <- audpc(means, dates, type="absolute")
  print(paste0("AUDPC for ",Genotypes[i], " is: ", AUDPC))
  Mock_AUDPC <-append(Mock_AUDPC,AUDPC)
  
  # #PLOT AS THE LOOP RUNS - TEMPORARY
  # plot_data <- as.data.frame(cbind(dates, means))
  # plot <- ggplot(data=plot_data, mapping=aes(x=dates, y=means, )) + geom_line() + geom_point() + 
  #   annotate("text", x = 35, y = 3.5, label = paste0("AUDPC: ", as.character(AUDPC))) +
  #   geom_ribbon(aes(ymin = min(means), ymax = means), fill = "#1b98e0")
  # plot <- plot + labs(title = paste0(Genotypes[i], " Mock Innoculation Average Disease Progression"))
  # show(plot)
}
Mock_Average_Path_Scores <- Mock_Average_Path_Scores[-1, ]
Mock_Average_Path_Scores <- as.data.frame(Mock_Average_Path_Scores)
print(Mock_Average_Path_Scores)


# Innoculated means
Inno_Average_Path_Scores <- matrix(data = NA, nrow=1, ncol=6, dimnames = NULL)
for(i in 1:GenoNumber){
  New_Row <- data.frame()
  Innotype <- subset(Inno_Sub, Genotype==Genotypes[i])
  #Calculate audpc using agricolae on the subset then leave room for the plots, same for innoculated
  print(paste0("Calculating averages for Innoculated", Genotypes[i]))
  dates <- c(28,35,49,56)
  means <- colMeans(Innotype[5:8])
  New_Row <- means
  New_Row$Genotype <- Genotypes[i]
  New_Row$Treatment <- c(unique(Innotype$treatment.))
  Inno_Average_Path_Scores <- rbind(Inno_Average_Path_Scores, New_Row, deparse.level = 0)
  AUDPC <- audpc(means, dates, type="absolute")
  print(paste0("AUDPC for ",Genotypes[i], " is: ", AUDPC))
  Inno_AUDPC <-append(Inno_AUDPC,AUDPC)
  # #PLOT AS THE LOOP RUNS - TEMPORARY
  # plot_data <- as.data.frame(cbind(dates, means))
  # plot <- ggplot(data=plot_data, mapping=aes(x=dates, y=means, )) + geom_line() + geom_point() + 
  #   annotate("text", x = 35, y = 3.5, label = paste0("AUDPC: ", as.character(AUDPC))) +
  #   geom_ribbon(aes(ymin = min(means), ymax = means), fill = "#1b98e0")
  # plot <- plot + labs(title = paste0(Genotypes[i], " Verticilium Innoculation Average Disease Progression"))
  # show(plot)
}
Inno_Average_Path_Scores <- Inno_Average_Path_Scores[-1, ]
Inno_Average_Path_Scores <- as.data.frame(Inno_Average_Path_Scores)
print(Inno_Average_Path_Scores)

#Create AUDPC results data frames
Mock_results <- cbind(Genotypes, Mock_AUDPC)
Inno_results <- cbind(Genotypes, Inno_AUDPC)
print(Mock_results)
print(Inno_results)
#Save Results
Save_File_Mock <- "/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Combined Average Plots/MockAUDPC.csv"
Save_File_Inno <- "/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Combined Average Plots/InnoAUDPC.csv"
if(file.exists(Save_File_Mock) == TRUE){
  file.remove(Save_File_Mock)
}
if(file.exists(Save_File_Inno) == TRUE){
  file.remove(Save_File_Inno)
}
write.csv(x = Mock_results, file = Save_File_Mock )
write.csv(x = Inno_results, file = Save_File_Inno)

#Convert AUDPC results to data frames:
Mock_results <- as.data.frame(Mock_results)
Inno_results <- as.data.frame(Inno_results)

##Plot Data Here
#Insert AUDPC plot for Mock and Innoculated here
# Mock
#define data to plot
x <- dates
y1 <- subset(Mock_Average_Path_Scores, Genotype=="Col0")
y1 <- unlist(y1[1:4])

y2 <- subset(Mock_Average_Path_Scores, Genotype=="FP34-P1")
y2 <- unlist(y2[1:4])

y3 <- subset(Mock_Average_Path_Scores, Genotype=="GG2-272")
y3 <- unlist(y3[1:4])

y4 <- subset(Mock_Average_Path_Scores, Genotype=="FP6-P1")
y4 <- unlist(y4[1:4])

y5 <- subset(Mock_Average_Path_Scores, Genotype=="FP1-P1")
y5 <- unlist(y5[1:4])

y6 <- subset(Mock_Average_Path_Scores, Genotype=="GG1-64")
y6 <- unlist(y6[1:4])

y7 <- subset(Mock_Average_Path_Scores, Genotype=="FP4-P1")
y7 <- unlist(y7[1:4])

y8 <- subset(Mock_Average_Path_Scores, Genotype=="FP6-P4")
y8 <- unlist(y8[1:4])

#plot first line
plot(x, y1, type='b', col='red', xlab='day', ylab='Disease Index Score', main='Mock Innoculation Disease Progression', xlim=c(28,63), ylim=c(1,5))

#add other lines to plot
lines(x, y2, col='yellow', type = 'b')
lines(x, y3, col='orange', type = 'b')
lines(x, y4, col='green', type = 'b')
lines(x, y5, col='pink', type = 'b')
lines(x, y6, col='black', type = 'b')
lines(x, y7, col='blue', type = 'b')
lines(x, y8, col='purple', type = 'b')

# Shade the area under the disease progression curves
polygon(c(x, rev(x)), c(y1, rev(rep(0, length(y1)))), col=adjustcolor('red', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y2, rev(rep(0, length(y2)))), col=adjustcolor('yellow', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y3, rev(rep(0, length(y3)))), col=adjustcolor('orange', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y4, rev(rep(0, length(y4)))), col=adjustcolor('green', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y5, rev(rep(0, length(y5)))), col=adjustcolor('pink', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y6, rev(rep(0, length(y6)))), col=adjustcolor('black', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y7, rev(rep(0, length(y7)))), col=adjustcolor('blue', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y8, rev(rep(0, length(y8)))), col=adjustcolor('purple', alpha.f=0.1), border=NA)
# alpha.f adjusts the transparency

# Label with AUDPC values
# Add labels for each curve
Mock_AUDPCs <- Mock_results$Mock_AUDPC
Mock_AUDPCs <- round(as.numeric(Mock_AUDPC), digits = 2)
# text(x[length(x)], y1[length(y1)], Mock_AUDPCs[1], pos=4, col='red')
# text(x[length(x)], y2[length(y2)], Mock_AUDPCs[2], pos=4, col='yellow')
# text(x[length(x)], y3[length(y3)], Mock_AUDPCs[3], pos=4, col='orange')
# text(x[length(x)], y4[length(y4)], Mock_AUDPCs[4], pos=4, col='green')
# text(x[length(x)], y5[length(y5)], Mock_AUDPCs[5], pos=4, col='pink')
# text(x[length(x)], y6[length(y6)], Mock_AUDPCs[6], pos=4, col='black')
# text(x[length(x)], y7[length(y7)], Mock_AUDPCs[7], pos=4, col='blue')
# text(x[length(x)], y8[length(y8)], Mock_AUDPCs[8], pos=4, col='purple')

# Add legend
legend("topleft", legend=Genotypes, col=c("red", "yellow", "orange", "green", "pink", "black", "blue", "purple"), lty=1, cex=0.8, title="Genotypes")
# Add AUDPC values
legend("topright",legend=Mock_AUDPCs, col=c("red", "yellow", "orange", "green", "pink", "black", "blue", "purple"), lty=1, cex=0.8, title="AUDPC Values")

##Inno AUDPC plot
#define data to plot
x <- dates
y1 <- subset(Inno_Average_Path_Scores, Genotype=="Col0")
y1 <- unlist(y1[1:4])

y2 <- subset(Inno_Average_Path_Scores, Genotype=="FP34-P1")
y2 <- unlist(y2[1:4])

y3 <- subset(Inno_Average_Path_Scores, Genotype=="GG2-272")
y3 <- unlist(y3[1:4])

y4 <- subset(Inno_Average_Path_Scores, Genotype=="FP6-P1")
y4 <- unlist(y4[1:4])

y5 <- subset(Inno_Average_Path_Scores, Genotype=="FP1-P1")
y5 <- unlist(y5[1:4])

y6 <- subset(Inno_Average_Path_Scores, Genotype=="GG1-64")
y6 <- unlist(y6[1:4])

y7 <- subset(Inno_Average_Path_Scores, Genotype=="FP4-P1")
y7 <- unlist(y7[1:4])

y8 <- subset(Inno_Average_Path_Scores, Genotype=="FP6-P4")
y8 <- unlist(y8[1:4])


#plot first line
plot(x, y1, type='b', col='red', xlab='day', ylab='Disease Index Score', main='Verticilium Innoculation Disease Progression', xlim=c(28,63), ylim=c(1,5))

#add other lines to plot
lines(x, y2, col='yellow', type = 'b')
lines(x, y3, col='orange', type = 'b')
lines(x, y4, col='green', type = 'b')
lines(x, y5, col='pink', type = 'b')
lines(x, y6, col='black', type = 'b')
lines(x, y7, col='blue', type = 'b')
lines(x, y8, col='purple', type = 'b')

# Shade the area under the disease progression curves
polygon(c(x, rev(x)), c(y1, rev(rep(0, length(y1)))), col=adjustcolor('red', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y2, rev(rep(0, length(y2)))), col=adjustcolor('yellow', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y3, rev(rep(0, length(y3)))), col=adjustcolor('orange', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y4, rev(rep(0, length(y4)))), col=adjustcolor('green', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y5, rev(rep(0, length(y5)))), col=adjustcolor('pink', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y6, rev(rep(0, length(y6)))), col=adjustcolor('black', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y7, rev(rep(0, length(y7)))), col=adjustcolor('blue', alpha.f=0.1), border=NA)
polygon(c(x, rev(x)), c(y8, rev(rep(0, length(y8)))), col=adjustcolor('purple', alpha.f=0.1), border=NA)
# alpha.f adjusts the transparency

# Label with AUDPC values
# Add labels for each curve
Inno_AUDPCs <- Inno_results$Inno_AUDPC
Inno_AUDPCs <- round(as.numeric(Inno_AUDPCs), digits = 2)
# text(x[length(x)], y1[length(y1)], Inno_AUDPCs[1], pos=4, col='red')
# text(x[length(x)], y2[length(y2)], Inno_AUDPCs[2], pos=4, col='yellow')
# text(x[length(x)], y3[length(y3)], Inno_AUDPCs[3], pos=4, col='orange')
# text(x[length(x)], y4[length(y4)], Inno_AUDPCs[4], pos=4, col='green')
# text(x[length(x)], y5[length(y5)], Inno_AUDPCs[5], pos=4, col='pink')
# text(x[length(x)], y6[length(y6)], Inno_AUDPCs[6], pos=4, col='black')
# text(x[length(x)], y7[length(y7)], Inno_AUDPCs[7], pos=4, col='blue')
# text(x[length(x)], y8[length(y8)], Inno_AUDPCs[8], pos=4, col='purple')

# Add legend
legend("topleft", legend=Genotypes, col=c("red", "yellow", "orange", "green", "pink", "black", "blue", "purple"), lty=1, cex=0.8, title="Genotypes")
# Add AUDPC values
legend("topright",legend=Inno_AUDPCs, col=c("red", "yellow", "orange", "green", "pink", "black", "blue", "purple"), lty=1, cex=0.8, title="AUDPC Values")

## Historgram of AUDPC and Standard Deviation
#prepare the AUDPC barplot data frame
combo_barplotdata <- cbind(Mock_results, Inno_results[2])
colnames(combo_barplotdata) <- c("Genotypes","Mock AUDPC", "Inoculated AUDPC")

# convert the results into suitable formats for ggplot:
# categorical data for Genotypes
# Convert to character vector and then to factor if needed
combo_barplotdata$Genotypes <- as.factor(as.character(combo_barplotdata$Genotypes))

# Check the structure of Genotypes
str(combo_barplotdata$Genotypes)
# numerical data 
combo_barplotdata$`Mock AUDPC` <- as.numeric(combo_barplotdata$`Mock AUDPC`)
combo_barplotdata$`Inoculated AUDPC` <- as.numeric(combo_barplotdata$`Inoculated AUDPC`)

## Corrected Bar Plot
# Reshape data into long format
combo_barplotdata_long <- melt(combo_barplotdata, id.vars = "Genotypes")
# melt function from reshape2 package converts the data from a wide to a long format (A long format contains values that do repeat in the first column)
# Plot the side-by-side stacked bar plot

# Calculate Standard Error:
standard_error_data <- combo_barplotdata_long %>%
  group_by(variable) %>%
  summarise(Standard_Error = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))))

# Merge Standard Error data back into combo_barplotdata_long dataset
combo_barplotdata_long <- merge(combo_barplotdata_long, standard_error_data, by = "Genotypes", all.x = TRUE)

#calculate the ymin and ymax values, put them in the Error_Bar_Data frame and try to just set ymin and ymax to these columns


# calculate the length of the datasets using Genotypes
## Now need to figure out how to add error bars using the SE values held in the combobarplot table
Data_len <- length(Genotypes)

AUDPC_Plot <- ggplot(data = combo_barplotdata_long, aes(x = Genotypes, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  labs(title = "AUDPC of Mock vs Inoculated",
       x = "Genotypes", y = "Average AUDPC", fill = "Treatment") +
  scale_fill_manual(values = c("green", "red")) +
  geom_errorbar(aes(ymin = value - Standard_Error, ymax = value + Standard_Error),position = position_dodge(width = 1), 
                width=0.2, colour="black", alpha=0.5, linewidth=0.5)


show(AUDPC_Plot)
#geomerrorbar() needs to be used but it needs to be able to work on both bars