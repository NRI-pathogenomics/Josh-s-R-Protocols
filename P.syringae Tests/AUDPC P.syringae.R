#AUDPC P.syringae
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
Path_Assay <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/P.syringae Tests/Disease severity Scores.csv", 
                       header = TRUE, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE, comment.char = "")
Path_Assay <- setNames(Path_Assay, nm=c("Replicate", "Treatment", "Block", "Day 0", "Day 1", "Day 2", "Day 3", "Day 5"))


if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/P.syringae Tests/Mock Averaged Results")==FALSE){
  file.create("Mock Averaged Results")
}

if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/P.syringae Tests/Innoculated Averaged Results")==FALSE){
  file.create("Innoculated Averaged Results")
}
##Subsetting the Data
Inno_Sub <- subset(Path_Assay, Treatment=="I")
Mock_Sub <- subset(Path_Assay, Treatment=="M")
##Individual AUDPC
Tail_Value <- Path_Assay$block[length(Path_Assay$block)]

setNames(Path_Assay, nm=c("Replicate", "Treatment", "Block", "Day 0", "Day 1", "Day 2", "Day 3", "Day 5"))


#Calculate audpc using agricolae on the subset then leave room for the plots, same for innoculated
# Mock Results
MockLength <- length(Mock_Sub$Replicate)
Mock_AUDPC <- list()
for(i in 1:MockLength){
  Mock_Rep <- Mock_Sub[i,]
  dates <- c(0,1,2,3,5)
  evaluation <- unlist(Mock_Rep[4:8], use.names=FALSE)
  AUDPC <- audpc(evaluation, dates, type="absolute")
  Mock_AUDPC <- c(Mock_AUDPC, AUDPC)
}
Mock_Sub$AUDPC <- Mock_AUDPC

#Inocculated Results
InnoLength <- length(Inno_Sub$Replicate)
Inno_AUDPC <- list()
for(i in 1:InnoLength){
  Inno_Rep <- Inno_Sub[i,]
  dates <- c(0,1,2,3,5)
  evaluation <- unlist(Inno_Rep[4:8], use.names=FALSE)
  AUDPC <- audpc(evaluation, dates, type="absolute")
  Inno_AUDPC <- c(Inno_AUDPC, AUDPC)
}
Inno_Sub$AUDPC <- Inno_AUDPC


## Calculate the Mean values
# reuse Mock_sub and Inno_sub
Mock_means <- data.frame()
Mock_AUDPC <- list()
Inno_means <- data.frame()
Inno_AUDPC <- list()
## check if mock output files exist and create them if not in the Onedrive local repository
if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/P.syrinage Tests/Mock Averaged Results")==FALSE){
  file.create("Mock Averaged Results")
}

if(file.exists("/Users/joshhoti/Library/CloudStorage/
               OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/P.syringae/Innoculated Averaged Results")==FALSE){
  file.create("Innoculated Averaged Results")
}
## Calculate and save the Averaged AUPDC results
Mock_Sub$AUDPC <- as.numeric(Mock_Sub$AUDPC)
Inno_Sub$AUDPC <- as.numeric(Inno_Sub$AUDPC)
Mock_Average_AUDPC <- mean(Mock_Sub$AUDPC)
Inoculated_Average_ADUPC <- mean(Inno_Sub$AUDPC)
AUDPC_results <- rbind(Mock_Average_AUDPC, Inoculated_Average_ADUPC)
AUDPC_results <- `colnames<-`(AUDPC_results, "value")


standard_error_data <- list()

for(i in 1:MockLength){
  #Mock
  Mock_AUDPC <-as.numeric(Mock_AUDPC)
  mock.st.dev <- sd(Mock_AUDPC)
  print(paste("Standard Deviation: ", mock.st.dev))
  print(paste("number of reps: ", MockLength))
  mock_se <- mock.st.dev/sqrt(MockLength)
  print(paste("SE value ",mock_se))}

for(i in 1:InnoLength){
  #Inoculated
  Inno_AUDPC <-as.numeric(Inno_AUDPC)
  inno.st.dev <- sd(Inno_AUDPC)
  print(paste("Standard Deviation: ", inno.st.dev))
  print(paste("number of reps: ", InnoLength))
  inno_se <- inno.st.dev/sqrt(InnoLength)
  print(paste("SE value ",inno_se))}

standard_error_data <- rbind(mock_se, inno_se)
standard_error_data <- `colnames<-`(standard_error_data, "SE")
## Corrected Bar Plot

# melt function from reshape2 package converts the data from a wide to a long format (A long format contains values that do repeat in the first column)
# Plot the side-by-side stacked bar plot

## Corrected Bar Plot
Treatment <- c("Mock", "Inoculated")
barplotdata <- cbind(Treatment, AUDPC_results,standard_error_data)
barplotdata <- as.data.frame(barplotdata)
barplotdata$value <- as.numeric(barplotdata$value)
barplotdata$SE <- as.numeric(barplotdata$SE)
library(ggplot2)

# Create color mapping
color_mapping <- c("Mock" = "#1E90FF", "Inoculated" = "#FF6347")

# Create the plot with horizontal bars
AUDPC_Plot <- ggplot(data = barplotdata, aes(x = Treatment, y = value, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.3) +
  geom_errorbar(aes(ymin = value - SE, ymax = value + SE), 
                position = position_dodge(width = 0.9), 
                width = 0.05, 
                colour = "black", 
                alpha = 0.7, 
                linewidth = 0.7) +
  labs(
    title = "AUDPC Values for Col-0 Infected with P.syringae",
    x = "Treatment", 
    y = "Average AUDPC Value"
  ) +
  scale_fill_manual(values = color_mapping) +
  theme_minimal() +
  theme(
    legend.position = "none"  # Remove legend if you want
  )

# Display the plot

show(AUDPC_Plot)

#geomerrorbar() needs to be used but it needs to be able to work on both bars

# ##Plot Data Here
# #Insert AUDPC plot for Mock and Innoculated here
# # Mock
# #define data to plot
# x <- dates
# y1 <- subset(Mock_Average_Path_Scores, Genotype=="Col0")
# y1 <- unlist(y1[1:4])
# 
# y2 <- subset(Mock_Average_Path_Scores, Genotype=="FP34-P1")
# y2 <- unlist(y2[1:4])
# 
# y3 <- subset(Mock_Average_Path_Scores, Genotype=="GG2-272")
# y3 <- unlist(y3[1:4])
# 
# y4 <- subset(Mock_Average_Path_Scores, Genotype=="FP6-P1")
# y4 <- unlist(y4[1:4])
# 
# y5 <- subset(Mock_Average_Path_Scores, Genotype=="FP1-P1")
# y5 <- unlist(y5[1:4])
# 
# y6 <- subset(Mock_Average_Path_Scores, Genotype=="GG1-64")
# y6 <- unlist(y6[1:4])
# 
# y7 <- subset(Mock_Average_Path_Scores, Genotype=="FP4-P1")
# y7 <- unlist(y7[1:4])
# 
# y8 <- subset(Mock_Average_Path_Scores, Genotype=="FP6-P4")
# y8 <- unlist(y8[1:4])
# 
# #plot first line
# plot(x, y1, type='b', col='red', xlab='day', ylab='Disease Index Score', main='Mock Innoculation Disease Progression', xlim=c(28,63), ylim=c(1,5))
# 
# #add other lines to plot
# lines(x, y2, col='yellow', type = 'b')
# lines(x, y3, col='orange', type = 'b')
# lines(x, y4, col='green', type = 'b')
# lines(x, y5, col='pink', type = 'b')
# lines(x, y6, col='black', type = 'b')
# lines(x, y7, col='blue', type = 'b')
# lines(x, y8, col='purple', type = 'b')
# 
# # Shade the area under the disease progression curves
# polygon(c(x, rev(x)), c(y1, rev(rep(0, length(y1)))), col=adjustcolor('red', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y2, rev(rep(0, length(y2)))), col=adjustcolor('yellow', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y3, rev(rep(0, length(y3)))), col=adjustcolor('orange', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y4, rev(rep(0, length(y4)))), col=adjustcolor('green', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y5, rev(rep(0, length(y5)))), col=adjustcolor('pink', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y6, rev(rep(0, length(y6)))), col=adjustcolor('black', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y7, rev(rep(0, length(y7)))), col=adjustcolor('blue', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y8, rev(rep(0, length(y8)))), col=adjustcolor('purple', alpha.f=0.1), border=NA)
# # alpha.f adjusts the transparency
# 
# # Label with AUDPC values
# # Add labels for each curve
# Mock_AUDPCs <- Mock_results$Mock_AUDPC
# Mock_AUDPCs <- round(as.numeric(Mock_AUDPC), digits = 2)
# # text(x[length(x)], y1[length(y1)], Mock_AUDPCs[1], pos=4, col='red')
# # text(x[length(x)], y2[length(y2)], Mock_AUDPCs[2], pos=4, col='yellow')
# # text(x[length(x)], y3[length(y3)], Mock_AUDPCs[3], pos=4, col='orange')
# # text(x[length(x)], y4[length(y4)], Mock_AUDPCs[4], pos=4, col='green')
# # text(x[length(x)], y5[length(y5)], Mock_AUDPCs[5], pos=4, col='pink')
# # text(x[length(x)], y6[length(y6)], Mock_AUDPCs[6], pos=4, col='black')
# # text(x[length(x)], y7[length(y7)], Mock_AUDPCs[7], pos=4, col='blue')
# # text(x[length(x)], y8[length(y8)], Mock_AUDPCs[8], pos=4, col='purple')
# 
# # Add legend
# legend("topleft", legend=Genotypes, col=c("red", "yellow", "orange", "green", "pink", "black", "blue", "purple"), lty=1, cex=0.8, title="Genotypes")
# # Add AUDPC values
# legend("topright",legend=Mock_AUDPCs, col=c("red", "yellow", "orange", "green", "pink", "black", "blue", "purple"), lty=1, cex=0.8, title="AUDPC Values")
# 
# ##Inno AUDPC plot
# #define data to plot
# x <- dates
# y1 <- subset(Inno_Average_Path_Scores, Genotype=="Col0")
# y1 <- unlist(y1[1:4])
# 
# y2 <- subset(Inno_Average_Path_Scores, Genotype=="FP34-P1")
# y2 <- unlist(y2[1:4])
# 
# y3 <- subset(Inno_Average_Path_Scores, Genotype=="GG2-272")
# y3 <- unlist(y3[1:4])
# 
# y4 <- subset(Inno_Average_Path_Scores, Genotype=="FP6-P1")
# y4 <- unlist(y4[1:4])
# 
# y5 <- subset(Inno_Average_Path_Scores, Genotype=="FP1-P1")
# y5 <- unlist(y5[1:4])
# 
# y6 <- subset(Inno_Average_Path_Scores, Genotype=="GG1-64")
# y6 <- unlist(y6[1:4])
# 
# y7 <- subset(Inno_Average_Path_Scores, Genotype=="FP4-P1")
# y7 <- unlist(y7[1:4])
# 
# y8 <- subset(Inno_Average_Path_Scores, Genotype=="FP6-P4")
# y8 <- unlist(y8[1:4])
# 
# 
# #plot first line
# plot(x, y1, type='b', col='red', xlab='day', ylab='Disease Index Score', main='Verticilium Innoculation Disease Progression', xlim=c(28,63), ylim=c(1,5))
# 
# #add other lines to plot
# lines(x, y2, col='yellow', type = 'b')
# lines(x, y3, col='orange', type = 'b')
# lines(x, y4, col='green', type = 'b')
# lines(x, y5, col='pink', type = 'b')
# lines(x, y6, col='black', type = 'b')
# lines(x, y7, col='blue', type = 'b')
# lines(x, y8, col='purple', type = 'b')
# 
# # Shade the area under the disease progression curves
# polygon(c(x, rev(x)), c(y1, rev(rep(0, length(y1)))), col=adjustcolor('red', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y2, rev(rep(0, length(y2)))), col=adjustcolor('yellow', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y3, rev(rep(0, length(y3)))), col=adjustcolor('orange', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y4, rev(rep(0, length(y4)))), col=adjustcolor('green', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y5, rev(rep(0, length(y5)))), col=adjustcolor('pink', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y6, rev(rep(0, length(y6)))), col=adjustcolor('black', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y7, rev(rep(0, length(y7)))), col=adjustcolor('blue', alpha.f=0.1), border=NA)
# polygon(c(x, rev(x)), c(y8, rev(rep(0, length(y8)))), col=adjustcolor('purple', alpha.f=0.1), border=NA)
# # alpha.f adjusts the transparency
# 
# # Label with AUDPC values
# # Add labels for each curve
# Inno_AUDPCs <- Inno_results$Inno_AUDPC
# Inno_AUDPCs <- round(as.numeric(Inno_AUDPCs), digits = 2)
# # text(x[length(x)], y1[length(y1)], Inno_AUDPCs[1], pos=4, col='red')
# # text(x[length(x)], y2[length(y2)], Inno_AUDPCs[2], pos=4, col='yellow')
# # text(x[length(x)], y3[length(y3)], Inno_AUDPCs[3], pos=4, col='orange')
# # text(x[length(x)], y4[length(y4)], Inno_AUDPCs[4], pos=4, col='green')
# # text(x[length(x)], y5[length(y5)], Inno_AUDPCs[5], pos=4, col='pink')
# # text(x[length(x)], y6[length(y6)], Inno_AUDPCs[6], pos=4, col='black')
# # text(x[length(x)], y7[length(y7)], Inno_AUDPCs[7], pos=4, col='blue')
# # text(x[length(x)], y8[length(y8)], Inno_AUDPCs[8], pos=4, col='purple')
# 
# # Add legend
# legend("topleft", legend=Genotypes, col=c("red", "yellow", "orange", "green", "pink", "black", "blue", "purple"), lty=1, cex=0.8, title="Genotypes")
# # Add AUDPC values
# legend("topright",legend=Inno_AUDPCs, col=c("red", "yellow", "orange", "green", "pink", "black", "blue", "purple"), lty=1, cex=0.8, title="AUDPC Values")
# 
# ## Historgram of AUDPC and Standard Deviation
# #Reshape the data to have column stating both the Genotype and the treatment and then merge the AUDPC values (In a new combo_data plot)
# Mock_column <- as.data.frame(paste(Genotypes, " Mock Treatment"))
# 
# Mock_column$AUDPC <- unlist(Mock_results$Mock_AUDPC)
# 
# Mock_column <- setNames(Mock_column, nm = c("Genotype + Treatment", "AUDPC"))
# 
# Inno_column <- as.data.frame(paste(Genotypes, " Inocculated Treatment"))
# 
# Inno_column$AUDPC <- unlist(Inno_results$Inno_AUDPC)
# 
# Inno_column <- setNames(Inno_column, nm = c("Genotype + Treatment", "AUDPC"))
# 
# print(Mock_column)
# print(Inno_column)
# 
# combo_barplotdata <- rbind(Mock_column, Inno_column)
# 
# # When it comes to the Standard Error I need to filter through the individual AUDPC replicates data to get the number of observations per per genotype
# # add a number of replicates per treatment to the data set
# 
# # Calculate Standard Error:
# 
# standard_error_data <- list()
# 
# # Reshape data into long format
# combo_barplotdata_long <- melt(combo_barplotdata, id.vars = "Genotype + Treatment")
# combo_barplotdata$`Genotype + Treatment` <- as.factor(as.character(combo_barplotdata$`Genotype + Treatment`))
# 
# # Check the structure of Genotypes
# str(combo_barplotdata_long$`Genotype + Treatment`)
# # numerical data 
# combo_barplotdata_long$value <- as.numeric(combo_barplotdata_long$value)
# 
# # Check the structure of Genotypes
# str(combo_barplotdata_long$`Genotype + Treatment`)
# # convert value to numerical data 
# combo_barplotdata_long$value <- as.numeric(combo_barplotdata_long$value)
# 
# #use the loop reliably calculate the SE value for each treatment using the INDVIDUAL AUDPC data
# 
# # In the loop create a subset of the Results data frame, first for the Mock then for the Innoculated

