#need to put this in for version 3.5 of R
#you need to download an old version of the blockdesign package or I can send you it,
#then specify the location you saved the unzipped file eg ~/Documents below
install.packages("~/Documents/blocksdesign_2.7.tar.gz",repos=NULL,type="source")
require(blocksdesign)
setwd("/Users/folder_where_file_is")
#the following is a file with genotypes and a number associated with each
number_for_geno<-read.csv("numbers.csv", header=TRUE)
# change numbers below to suit your randomisation
RBC<- blocks(treatments=386,replicates=5)
RBC
RD<-RBC$Design
# combine design with your genotype names 
comb<- merge(RD,number_for_geno, by.x='Treatments', by.y='Number',all.x=TRUE)
comb_o<- comb[order(comb$Plots),]
write.csv(comb_o,"randomisation.csv")