# Col-0 Batch 2 Mixed Model Analysis
# USE The REML results to figure out which model best explains the data (the lower the REML the better the model fit)
install.packages("ordinal")
library(ordinal)
# since the response variables are ordinal data
# # Install and load required package
# if(!require(vcd)) install.packages("vcd")
# library(vcd)
# 
# # Calculate association measures
# assocstats(table(disease_scores$Treatment, disease_scores$Infiltrated.with.P.syringae))
# assocstats(table(disease_scores$Treatment, disease_scores$Fungus.gnats))
# assocstats(table(disease_scores$Treatment, disease_scores$Perforation))
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
if(("FSA" %in% all_packages)==FALSE){
  install.packages("FSA")}
if(("dunn.test" %in% all_packages)==FALSE){
  install.packages("dunn.test")}
if(("rcompanion" %in% all_packages)==FALSE){
  install.packages("rcompanion")}
install.packages("ordinal")
install.packages("multcompView")
library(multcompView)
library(rcompanion)
library(ordinal)
library(lme4)
# since the response variables are ordinal data
disease_scores <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 2/Batch 2 Scores.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")
# data processing
disease_scores <- na.omit(disease_scores) #removes NA values
disease_scores <- subset(disease_scores, select = -c(Chlorosis,Random, Block.Rep)) 
disease_scores$Treatment <- gsub("-", ".", disease_scores$Treatment)
#removes chlorosis as this test is focusing on leaf damage
#randomized block design calculations and the block rep (not important for this model)

## Damage and chlorosis scores are RESPONSE variables so they will be modeled separately for the cld test

##Infiltration with P.syringae Complete Model
lmer1a <- lmer(Leaf.Damage ~ Treatment + Infiltrated.with.P.syringae + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation), data = disease_scores)
summary(lmer1a)

# Simpler Model (dropped Infiltrated.with.P.syringae)
lmer1b <- lmer(Leaf.Damage ~ Treatment + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation), data = disease_scores)
summary(lmer1b)

anova(lmer1a,lmer1b)
# compare whether model with block is significantly different to the model without - if it is then P.syringae is important 
#It is in this case the Chisq = 0.1409 and p-value = 0.7074 (P.value < 0.05) so including P.syrinage does not improve the model

# Simpler Model (dropped Block as a Random effect)

lmer1c <- lmer(Leaf.Damage ~ Treatment + (1 | Fungus.gnats) + (1 | Perforation), data = disease_scores)
summary(lmer1c)
anova(lmer1b,lmer1c)
#results:
#        Chisq  Pr(>Chisq)
# lmer1b  0.01  0.9203
# inclusion of Block does not improve the model

# Simpler model without Perforation
lmer1d <- lmer(Leaf.Damage ~ Treatment + (1 | Fungus.gnats), data = disease_scores)
summary(lmer1d)
anova(lmer1c,lmer1d)
#results:
#          Chisq | Pr(>Chisq)   
# lmer1c: 6.7517 | 0.009366 ** - model c explains the model well, so inclusion of perforation does significantly improve the model

#Simpler model without Fungus gnats

lmer1e <- lmer(Leaf.Damage ~ Treatment + (1 | Perforation), data = disease_scores)
summary(lmer1e)
anova(lmer1c,lmer1e)
#results
#         Chisq | Pr(>Chisq)    
# lmer1c 20.48  | 6.025e-06 *** - shows the inclusion of fungus gnats significantly improves the model so lmer1c better fits

#Simplier model without Treatment

lmer1f <- lmer(Leaf.Damage ~ (1 | Fungus.gnats) + (1 | Perforation), data = disease_scores)
anova(lmer1c, lmer1f)
#results
#            Chisq | Pr(>Chisq)
# lmer1c    1.4447 | 0.8364.   Results indicate that treatment does not explain a significant amount of variance in leaf damage

#best fit model:
summary(lmer1f)

#Now we need a post-hoc test to determine which treatments differ from one another:
# the magnitude of T-values indicates that at least one of the treatments differs from the others in terms of Leaf Damage but the post-hoc test will reveal which treatment differs
#In short: Treatment is a fixed effect and one or more treatment is behind the leaf damage
#Fungus.gnat damage is accounted for as a random effect that causes variance in the data
# but depending on the results of the post-hoc test, if one of the infected/agroinfiltrated treatments differs signficantly from the others then it/they are the reason for leaf degradation - ergo the stats will show if P.syringae presence is behind leaf degredation

# Null model (hypothesis: no factor is more closely associated with damage)
model_null <- clm(as.ordered(Leaf.Damage) ~ 1, 
                  data = disease_scores)

# # Compare using likelihood ratio tests using a default P.value of < 0.05


#Post Hoc Test - Tukey Test
library(emmeans)
posthoc <- emmeans(lmer1e, ~ Treatment)
summary(posthoc)
tukey_results <- pairs(posthoc, adjust = "tukey")
summary(tukey_results)
tukey_results <- as.data.frame(tukey_results)
P.syrinage_cld <- cldList(p.value ~ contrast, data = tukey_results, threshold = 0.05)


