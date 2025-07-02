# Col-0 Batch 2 Mixed Model Analysis
# USE The REML results to figure out which model best explains the data (the lower the REML the better the model fit)
# install.packages("ordinal")
# since the response variables are ordinal data
# # Install and load required package
# if(!require(vcd)) install.packages("vcd")
# library(vcd)
# 
# # Calculate association measures
# assocstats(table(disease_scores$Treatment, disease_scores$Infiltrated.with.P.syringae))
# assocstats(table(disease_scores$Treatment, disease_scores$Fungus.gnats))
# assocstats(table(disease_scores$Treatment, disease_scores$Perforation))
# if(("dplyr" %in% all_packages)==FALSE){
#   install.packages("dplyr")}
# if(("agricolae" %in% all_packages)==FALSE){
#   install.packages("agricolae")}
# if(("agricolaeplotr" %in% all_packages)==FALSE){
#   install.packages("agricolaeplotr")}
# if(("tidyverse" %in% all_packages)==FALSE){
#   install.packages("tidyverse")}
# if(("plotrix" %in% all_packages)==FALSE){
#   install.packages("plotrix")}
# if(("FSA" %in% all_packages)==FALSE){
#   install.packages("FSA")}
# if(("dunn.test" %in% all_packages)==FALSE){
#   install.packages("dunn.test")}
# if(("rcompanion" %in% all_packages)==FALSE){
#   install.packages("rcompanion")}
# install.packages("ordinal")
# install.packages("multcompView")
library(ordinal)
library(multcompView)
library(rcompanion)
library(ordinal)
library(lme4)
# since the response variables are ordinal data
disease_scores <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 2/Batch 2 rescores.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")
# data processing
damage_scores <- na.omit(disease_scores) #removes NA values
damage_scores <- subset(damage_scores, select = -c(X5.dpi.Chlorosis,Random, Block.Rep)) 
damage_scores$Treatment <- gsub("-", ".", damage_scores$Treatment)
#removes X5.dpi.Chlorosis as this test is focusing on leaf damage
#randomized block design calculations and the block rep (not important for this model)

## Damage and X5.dpi.Chlorosis scores are RESPONSE variables so they will be modeled separately for the cld test

## Leaf Damage Complete Model
lmer1z <- lmer(X5.dpi.Leaf.Damage ~ Treatment + Infiltrated.with.P.syringae + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation) + (1|X0.dpi.Leaf.Damage), data = damage_scores)
summary(lmer1z)
## Leaf Damage (dropped x0.dpi Leaf.Damage)
lmer1a <- lmer(X5.dpi.Leaf.Damage ~ Treatment + Infiltrated.with.P.syringae + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation), data = damage_scores)
summary(lmer1a)

anova(lmer1z, lmer1a)

# Simpler Model (dropped Infiltrated.with.P.syringae)
lmer1b <- lmer(X5.dpi.Leaf.Damage ~ Treatment + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation), data = damage_scores)
summary(lmer1b)

anova(lmer1a,lmer1b)
# compare whether model with block is significantly different to the model without - if it is then P.syringae is important 
#It is in this case the Chisq = 0.1409 and p-value = 0.7074 (P.value < 0.05) so including P.syrinage does not improve the model

# Simpler Model (dropped Block as a Random effect)

lmer1c <- lmer(X5.dpi.Leaf.Damage ~ Treatment + (1 | Fungus.gnats) + (1 | Perforation), data = damage_scores)
summary(lmer1c)
anova(lmer1b,lmer1c)
#results:
#        Chisq  Pr(>Chisq)
# lmer1b  0.01  0.9203
# inclusion of Block does not improve the model

# Simpler model without Perforation
lmer1d <- lmer(X5.dpi.Leaf.Damage ~ Treatment + (1 | Fungus.gnats), data = damage_scores)
summary(lmer1d)
anova(lmer1c,lmer1d)
#results:
#model c explains the model well, so inclusion of perforation does significantly improve the model

#Simpler model without Fungus gnats

lmer1e <- lmer(X5.dpi.Leaf.Damage ~ Treatment + (1 | Perforation), data = damage_scores)
summary(lmer1e)
anova(lmer1c,lmer1e)
#r 
# lmer1c shows the inclusion of fungus gnats significantly improves the model so lmer1c better fits

#cannot filter anymore effects as Treatment is fixed and we need it for emmeans         
#best fit model:
summary(lmer1c)

#Now we need a post-hoc test to determine which treatments differ from one another:
# the magnitude of T-values indicates that at least one of the treatments differs from the others in terms of Leaf Damage but the post-hoc test will reveal which treatment differs
#In short: Treatment is a fixed effect and one or more treatment is behind the leaf damage
#Fungus.gnat damage is accounted for as a random effect that causes variance in the data
# but depending on the results of the post-hoc test, if one of the infected/agroinfiltrated treatments differs signficantly from the others then it/they are the reason for leaf degradation - ergo the stats will show if P.syringae presence is behind leaf degredation


#Post Hoc Test - Tukey Test
library(emmeans)
posthoc <- emmeans(lmer1c, ~ Treatment)
summary(posthoc)
tukey_results <- pairs(posthoc, adjust = "tukey")
summary(tukey_results)
tukey_results <- as.data.frame(tukey_results)
leaf_damage_cld <- cldList(p.value ~ contrast, data = tukey_results, threshold = 0.05)

## X5.dpi.Chlorosis Complete Model
# data processing
chlorosis_scores <- na.omit(disease_scores) #removes NA values
chlorosis_scores <- subset(disease_scores, select = -c(X5.dpi.Leaf.Damage,Random, Block.Rep)) 
chlorosis_scores$Treatment <- gsub("-", ".", chlorosis_scores$Treatment)
#removes Leaf Damage as this test is focusing on X5.dpi.Chlorosis
#randomized block design calculations and the block rep (not important for this model)

## Damage and X5.dpi.Chlorosis scores are RESPONSE variables so they will be modeled separately for the cld test

## X5.dpi.Chlorosis Complete Model
# lmer2z <- lmer(X5.dpi.Chlorosis ~ Treatment + Infiltrated.with.P.syringae + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation) + (1|X0.dpi.Chlorosis), data = chlorosis_scores)
# summary(lmer1z)
# this doesnt work as chlorosis on 0 dpi is consistently 0

# Simpler Model (dropped X0.dpi.Chlorosis)
lmer2a <- lmer(X5.dpi.Chlorosis ~ Treatment + Infiltrated.with.P.syringae + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation), data = chlorosis_scores)
summary(lmer2a)

anova(lmer2z, lmer2a)

# Simpler Model (dropped Infiltrated.with.P.syringae)
lmer2b <- lmer(X5.dpi.Chlorosis ~ Treatment + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation), data = chlorosis_scores)
summary(lmer2b)

anova(lmer2a,lmer2b)
# compare whether model with block is significantly different to the model without - if it is then P.syringae is important 
#results:
#          Chisq | Pr(>Chisq)
# lmer2a  0.9659 | 0.3257 - Indicates that "Infiltrated.with.P.syringae" does not significantly improve the model
# Simpler Model (dropped Block as a Random effect)

lmer2c <- lmer(X5.dpi.Chlorosis ~ Treatment + (1 | Fungus.gnats) + (1 | Perforation), data = chlorosis_scores)
summary(lmer2c)
anova(lmer2b,lmer2c)
#    Chisq | Pr(>Chisq)
# lmer2b 0 | 1 - results indicate "Block" does not significantly improve the model

# Simpler model without Perforation
lmer2d <- lmer(X5.dpi.Chlorosis ~ Treatment + (1 | Fungus.gnats), data = chlorosis_scores)
summary(lmer2d)
anova(lmer2c,lmer2d)
#    Chisq | Pr(>Chisq)
# lmer2c 0 | 1 results indicate "Perforation" does not significantly improve the model   

#Simpler model without Fungus gnats
# had to use lm() as the formula has removed all random effects
lmer2e <- lm(X5.dpi.Chlorosis ~ Treatment, data = chlorosis_scores)
summary(lmer2e)
anova(lmer2d,lmer2e)
#results
#       Chisq | Pr(>Chisq)
# lmer2d    0 | 1 - results indicate that "Fungus gnats" doesn't significantly improve the model

#best fit model:
summary(lmer2e)

#Now we need a post-hoc test to determine which treatments differ from one another:
# the magnitude of T-values indicates that at least one of the treatments differs from the others in terms of Leaf Damage but the post-hoc test will reveal which treatment differs
#In short: Treatment is a fixed effect and one or more treatment is behind the leaf damage
#Fungus.gnat damage is accounted for as a random effect that causes variance in the data
# but depending on the results of the post-hoc test, if one of the infected/agroinfiltrated treatments differs signficantly from the others then it/they are the reason for leaf degradation - ergo the stats will show if P.syringae presence is behind leaf degredation


#Post Hoc Test - Tukey Test
library(emmeans)
posthoc <- emmeans(lmer2e, ~ Treatment)
summary(posthoc)
tukey_results <- pairs(posthoc, adjust = "tukey")
summary(tukey_results)
tukey_results <- as.data.frame(tukey_results)
chlorosis_cld <- cldList(p.value ~ contrast, data = tukey_results, threshold = 0.05)