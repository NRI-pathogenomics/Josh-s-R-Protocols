# Col-0 Batch 7 Mixed Model Analysis
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
library(multcompView)
library(rcompanion)
library(ordinal)
library(lme4)
# since the response variables are ordinal data
disease_data <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 7/Batch 7 Disease.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")
# data processing
disease_scores <- na.omit(disease_data) #removes NA val, ues
disease_scores <- subset(disease_scores, select = -c(Agro..1.dpi.Chlorosis, Agro..5.dpi.Chlorosis, Random, Block.Rep)) 
disease_scores$Treatment <- gsub("-", ".", disease_scores$Treatment)
disease_scores <- subset(disease_scores, Treatment != "Ctrl.HP.I")
disease_scores$Block <- as.factor(disease_scores$Block)

# check the levels of all the factors in the data
# Shows number of unique levels for each column
sapply(disease_scores, function(x) length(unique(x)))

#Complete Model
#Dropped (1|Fungus gnats)
lmer1a <- lmer(PS..5.dpi.Leaf.Damage ~ Treatment + Infiltrated.with.P.syringae + (1 | Block) + (1 | Perforation) + (1|PS..0.dpi.Leaf.Damage), data = disease_scores)

summary(lmer1a)

# Simpler Model (dropped PS..0.dpi.Leaf.Damage as a random effect)

lmer1b <- lmer(PS..5.dpi.Leaf.Damage ~ Treatment + Infiltrated.with.P.syringae + (1 | Block) + (1 | Perforation), data = disease_scores)
summary(lmer1b)

anova(lmer1a, lmer1b)

# Simpler Model (dropped Perforation)
lmer1c <- lmer(PS..5.dpi.Leaf.Damage ~ Treatment + Infiltrated.with.P.syringae + (1 | Block), data = disease_scores)
summary(lmer1c)
anova(lmer1b,lmer1c)
# Simpler Model (dropped Infiltrated with P.syringae)

lmer1d <- lmer(PS..5.dpi.Leaf.Damage ~ Treatment  + (1 | Block), data = disease_scores)
summary(lmer1d)

anova(lmer1c,lmer1d)


#Simplier model without Treatment

lmer1e <- lmer(PS..5.dpi.Leaf.Damage ~ (1 | Block), data = disease_scores)
summary(lmer1e)
anova(lmer1d, lmer1e)

# Results indicate that treatment does explain a significant amount of variance in leaf damage - so lmer1d is the best model

#best fit model:
summary(lmer1d)

#No we need a post-hoc test to determine which treatments differ from one another:
# the magnitude of T-values indicates that at least one of the treatments differs from the others in terms of Leaf Damage but the post-hoc test will reveal which treatment differs
#In short: Treatment is a fixed effect and one or more treatment is behind the leaf damage
#Fungus.gnat damage is accounted for as a random effect that causes variance in the data
# but depending on the results of the post-hoc test, if one of the infected/agroinfiltrated treatments differs signficantly from the others then it/they are the reason for leaf degradation - ergo the stats will show if P.syringae presence is behind leaf degredation

# Null model (hypothesis: neither factor is more closely associated with damage)
model_null <- clm(as.ordered(PS..5.dpi.Leaf.Damage) ~ 1, 
                  data = disease_scores)

# # Compare using likelihood ratio tests using a default P.value of < 0.05
# anova(model_null, model_ps)
# anova(model_null, model_pf)
# anova(model_null, model_fg)
# 
# #summary to quantify the effect size of each factor
# summary(model_ps)
# summary(model_pf)
# summary(model_fg)
# # Remodel to look at the interaction between treatment and whether it was infiltrated with P.syringae, accounting for leaf damage caused by fungus gnats

#Post Hoc Test - Tukey Test
library(emmeans)
posthoc <- emmeans(lmer1d, ~ Treatment)
summary(posthoc)
tukey_results <- pairs(posthoc, adjust = "tukey")
summary(tukey_results)
tukey_results <- as.data.frame(tukey_results)
disease_treatments_cld <- cldList(p.value ~ contrast, data = tukey_results, threshold = 0.05)
# Change EHA15.I to EHA105.I
disease_treatments_cld$Group[disease_treatments_cld$Group == "EHA15.I"] <- "EHA105.I"

## Agro..5.dpi.Chlorosis Complete Model
# data processing
chlorosis_scores <- na.omit(disease_data) #removes NA values
chlorosis_scores <- subset(chlorosis_scores, select = -c(PS..5.dpi.Leaf.Damage,Random, Block.Rep)) 
chlorosis_scores$Treatment <- gsub("-", ".", chlorosis_scores$Treatment)
#removes Leaf Damage as this test is focusing on Agro..5.dpi.Chlorosis
#randomized block design calculations and the block rep (not important for this model)

## Damage and Agro..5.dpi.Chlorosis scores are RESPONSE variables so they will be modeled separately for the cld test
sapply(chlorosis_scores, function(x) length(unique(x)))

## Agro..5.dpi.Chlorosis Complete Model (Dropped (1|Fungus.gnats))
# Simpler Model (dropped Agro..1.dpi.Chlorosis)
lmer2b <- lmer(Agro..5.dpi.Chlorosis ~ Treatment + Infiltrated.with.P.syringae + (1 | Block) + (1 | Perforation), data = chlorosis_scores)
summary(lmer2b)

# Simpler Model (dropped Infiltrated.with.P.syringae)
lmer2c <- lmer(Agro..5.dpi.Chlorosis ~ Treatment + (1 | Block)  + (1 | Perforation), data = chlorosis_scores)
summary(lmer2c)

anova(lmer2b,lmer2c)
# compare whether model with block is significantly different to the model without - if it is then P.syringae is important 
#results:
#          Chisq | Pr(>Chisq)
# lmer2a  0.9659 | 0.3257 - Indicates that "Infiltrated.with.P.syringae" does not significantly improve the model
# Simpler Model (dropped Block as a Random effect)

lmer2d <- lmer(Agro..5.dpi.Chlorosis ~ Treatment + (1 | Perforation), data = chlorosis_scores)
summary(lmer2d)
anova(lmer2c,lmer2d)

# Simpler model without Perforation
#had to use lm as all random factors had been removed
lmer2e <- lm(Agro..5.dpi.Chlorosis ~ Treatment, data = chlorosis_scores)
summary(lmer2e)
anova(lmer2d,lmer2e)
#    Chisq | Pr(>Chisq)
# lmer2c 0 | 1 results indicate "Perforation" does not significantly improve the model   

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
