# Col-0 Batch 3 Mixed Model Analysis
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
disease_scores <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 3/Batch 3 Scores.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")
# data processing
disease_scores <- na.omit(disease_scores) #removes NA values
disease_scores <- subset(disease_scores, select = -c(Col.0..Rep, Random, Block)) 
disease_scores$Treatment <- gsub("-", ".", disease_scores$Treatment)

#removes chlorosis as this test is focusing on leaf damage
#randomized block design calculations and the block rep (not important for this model)

# #The model - models for individual factors (Perforation, P.syringae infiltration and fungus gnats) to determine which of the two is more closely associated with leaf damage
# 
# # Model with only P. syringae (hypothesis: P.syringae are more closely associated with damage)
# model_ps <- clm(as.ordered(X5.dpi.Leaf.Damage) ~ Infiltrated.with.P.syringae, 
#                 data = disease_scores)
# 
# # Model with only fungus gnats (hypothesis: fungus gnats are more closely associated with damage)
# model_fg <- clm(as.ordered(X5.dpi.Leaf.Damage) ~ Fungus.gnats, 
#                 data = disease_scores)
# 
# # Model with only Perforation (hypothesis: Infiltration process is closely associated with damage)
# model_pf <- clm(as.ordered(X5.dpi.Leaf.Damage) ~ Perforation, 
#                 data = disease_scores)

#Complete Model
lmer1a <- lmer(Rotted.Leaves ~ Treatment + (1 | Chlorotic.leaves), data = disease_scores)
summary(lmer1a)

# Simpler Model (dropped X..Chlorosis)

lmer1b <- lm(Rotted.Leaves ~ Treatment, data = disease_scores)
summary(lmer1b)

anova(lmer1a, lmer1b)
## Result
#          Chisq  Pr(>Chisq)
# lmer1a    0         1
# indicates that the number of chlorotic leaves does not significantly explain variation in the data

#No we need a post-hoc test to determine which treatments differ from one another:
# the magnitude of T-values indicates that at least one of the treatments differs from the others in terms of Leaf Damage but the post-hoc test will reveal which treatment differs
#In short: Treatment is a fixed effect and one or more treatment is behind the leaf damage
#Fungus.gnat damage is accounted for as a random effect that causes variance in the data
# but depending on the results of the post-hoc test, if one of the infected/agroinfiltrated treatments differs signficantly from the others then it/they are the reason for leaf degradation - ergo the stats will show if P.syringae presence is behind leaf degredation

# Null model (hypothesis: neither factor is more closely associated with damage)
model_null <- clm(as.ordered(Rotted.Leaves) ~ 1, 
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
posthoc <- emmeans(lmer1b, ~ Treatment)
summary(posthoc)
tukey_results <- pairs(posthoc, adjust = "tukey")
summary(tukey_results)
tukey_results <- as.data.frame(tukey_results)
disease_treatments_cld <- cldList(p.value ~ contrast, data = tukey_results, threshold = 0.05)
show(disease_treatments_cld)
