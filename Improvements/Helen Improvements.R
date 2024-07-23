
library(agricolae)
library(binhf)
library(dunn.test)
library(DescTools)
library(rcompanion)
library(FSA)
setwd("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R-Protocols/Improvements")
pheno<-read.csv("NEWPT2.csv")

# calculate the AUDPC values 
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

#check for normality
shapiro.test(resid(lm(ansc(pheno$audpc,200)~pheno$Genotype*pheno$treatment.)))
hist(resid(lm(ansc(pheno$audpc,150)~pheno$Genotype*pheno$treatment.)))

# the data is not normally distributed an anscombe transformation does not help (see above)
# the Anscombe transform, is a variance-stabilizing transformation that transforms a random variable with a Poisson distribution
# into one with an approximately standard Gaussian distribution.

pheno$Genotype<-gsub("-","",pheno$Genotype)
kw<-kruskal.test(pheno, formula=pheno$audpc ~ pheno$Genotype * pheno$treatment.)
kw
# a p value of p-value < 2.2e-16 indicates there is a difference between treatments
dunn_result2 <- dunnTest(pheno$audpc, g=interaction(pheno$Genotype, pheno$treatment.,method="bonferroni"))

CLD = cldList(P.adj ~ Comparison, data=dunn_result2$res)
CLD

#this output matches your graph