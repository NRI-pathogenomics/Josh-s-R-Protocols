#need to put this in for version 3.5 of R
#you need to download an old version of the blockdesign package or I can send you it,
#then specify the location you saved the unzipped file eg ~/Documents below
install.packages("blocksdesign")
require(blocksdesign)
setwd("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Additional Scripts.csv")
#the following is a file with genotypes and a number associated with each
number_for_geno<-read.csv("Col-0_numbers.csv", header=TRUE)
# change numbers below to suit your randomisation
RBC<- blocks(treatments=2,replicates=10)
RBC
RD<-RBC$Design
# combine design with your genotype names 
comb<- merge(RD,number_for_geno, by.x='treatments', by.y='Col_0.Rep',all.x=TRUE)
comb_o<- comb[order(comb$plots),]
write.csv(comb_o,"randomisation.csv")
