# Col-0 Batch 1 Mixed Model Analysis
# # Install and load required package
# if(!require(vcd)) install.packages("vcd")
# library(vcd)
# 
# # Calculate association measures
# assocstats(table(damage_scores$Treatment, damage_scores$Infiltrated.with.P.syringae))
# assocstats(table(damage_scores$Treatment, damage_scores$Fungus.gnats))
# assocstats(table(damage_scores$Treatment, damage_scores$Perforation))
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
disease_scores <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 1/Batch 1 Scores.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")
# data processing
damage_scores <- na.omit(damage_scores) #removes NA values
damage_scores <- subset(damage_scores, select = -c(Chlorosis,Random, Block.Rep)) 
damage_scores$Treatment <- gsub("-", ".", damage_scores$Treatment)
damage_scores <- subset(damage_scores, Treatment != "Ctrl.HP.I")
#removes chlorosis as this test is focusing on leaf damage
#randomized block design calculations and the block rep (not important for this model)

# #The model - models for individual factors (Perforation, P.syringae infiltration and fungus gnats) to determine which of the two is more closely associated with leaf damage
# 
# # Model with only P. syringae (hypothesis: P.syringae are more closely associated with damage)
# model_ps <- clm(as.ordered(X5.dpi.Leaf.Damage) ~ Infiltrated.with.P.syringae, 
#                 data = damage_scores)
# 
# # Model with only fungus gnats (hypothesis: fungus gnats are more closely associated with damage)
# model_fg <- clm(as.ordered(X5.dpi.Leaf.Damage) ~ Fungus.gnats, 
#                 data = damage_scores)
# 
# # Model with only Perforation (hypothesis: Infiltration process is closely associated with damage)
# model_pf <- clm(as.ordered(X5.dpi.Leaf.Damage) ~ Perforation, 
#                 data = damage_scores)

#Complete Model
lmer1a <- lmer(X5.dpi.Leaf.Damage ~ Treatment + Infiltrated.with.P.syringae + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation) + (1|X0.dpi.Leaf.Damage), data = damage_scores)
summary(lmer1a)

# Simpler Model (dropped X0.dpi.Leaf.Damage as a random effect)

lmer1b <- lmer(X5.dpi.Leaf.Damage ~ Treatment + Infiltrated.with.P.syringae + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation), data = damage_scores)
summary(lmer1b)

anova(lmer1a, lmer1b)

# Simpler Model (dropped Infiltrated.with.P.syringae)
lmer1c <- lmer(X5.dpi.Leaf.Damage ~ Treatment + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation), data = damage_scores)
summary(lmer1b)

# Simpler Model (dropped Block as a Random effect)

lmer1d <- lmer(X5.dpi.Leaf.Damage ~ Treatment + (1 | Fungus.gnats) + (1 | Perforation), data = damage_scores)
summary(lmer1c)

anova(lmer1b,lmer1c)
# compare whether meodle with block is significantly different to the model without - if it is then Block is important 
#It is in this case

lmer1e <- lmer(X5.dpi.Leaf.Damage ~ Treatment + (1 | Block) + (1 | Perforation), data = damage_scores)
summary(lmer1e)
anova(lmer1b,lmer1d)
#Compare the model with Block and Fungus.gnats to the model with Block only
#P.value indicates Fungus gnats is importance
#Simpler model without the Perforation

lmer1f <- lmer(X5.dpi.Leaf.Damage ~ Treatment + (1 | Block) + (1 | Fungus.gnats), data = damage_scores)
summary(lmer1f)
anova(lmer1d,lmer1e)
#Result indicates that perforation is not important - perforation does not explain a significant amount of variance in leaf damage

#Simplier model without Treatment

lmer1g <- lmer(X5.dpi.Leaf.Damage ~ (1 | Block) + (1 | Fungus.gnats), data = damage_scores)
anova(lmer1f, lmer1g)

# Results indicate that treatment does explain a significant amount of variance in leaf damage

#best fit model:
summary(lmer1f)

#No we need a post-hoc test to determine which treatments differ from one another:
# the magnitude of T-values indicates that at least one of the treatments differs from the others in terms of Leaf Damage but the post-hoc test will reveal which treatment differs
#In short: Treatment is a fixed effect and one or more treatment is behind the leaf damage
#Fungus.gnat damage is accounted for as a random effect that causes variance in the data
# but depending on the results of the post-hoc test, if one of the infected/agroinfiltrated treatments differs signficantly from the others then it/they are the reason for leaf degradation - ergo the stats will show if P.syringae presence is behind leaf degredation

# Null model (hypothesis: neither factor is more closely associated with damage)
model_null <- clm(as.ordered(X5.dpi.Leaf.Damage) ~ 1, 
                  data = damage_scores)

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
posthoc <- emmeans(lmer1f, ~ Treatment)
summary(posthoc)
tukey_results <- pairs(posthoc, adjust = "tukey")
summary(tukey_results)
tukey_results <- as.data.frame(tukey_results)
disease_treatments_cld <- cldList(p.value ~ contrast, data = tukey_results, threshold = 0.05)
# Change EHA15.I to EHA105.I
disease_treatments_cld$Group[disease_treatments_cld$Group == "EHA15.I"] <- "EHA105.I"

## X5.dpi.Chlorosis Complete Model
# data processing
chlorosis_scores <- na.omit(disease_scores) #removes NA values
chlorosis_scores <- subset(disease_scores, select = -c(Random, Block.Rep)) 
chlorosis_scores$Treatment <- gsub("-", ".", chlorosis_scores$Treatment)
#removes Leaf Damage as this test is focusing on X5.dpi.Chlorosis
#randomized block design calculations and the block rep (not important for this model)

## Damage and X5.dpi.Chlorosis scores are RESPONSE variables so they will be modeled separately for the cld test

## X5.dpi.Chlorosis Complete Model
lmer2z <- lmer(Chlorosis ~ Treatment + Infiltrated.with.P.syringae + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation), data = chlorosis_scores)
summary(lmer2z)
# Simpler Model (dropped (1 | Perforation))
lmer2a <- lmer(Chlorosis ~ Treatment + Infiltrated.with.P.syringae + (1 | Block) + (1 | Fungus.gnats), data = chlorosis_scores)
summary(lmer2a)

anova(lmer2z, lmer2a)

# Simpler Model (dropped Infiltrated.with.P.syringae)
lmer2b <- lmer(Chlorosis ~ Treatment + (1 | Block) + (1 | Fungus.gnats), data = chlorosis_scores)
summary(lmer2b)

anova(lmer2a,lmer2b)
# Simpler model - dropped block
lmer2c <- lmer(Chlorosis ~ Treatment + (1 | Fungus.gnats), data = chlorosis_scores)
summary(lmer2c)
anova(lmer2b,lmer2c)
#    Chisq | Pr(>Chisq)
# lmer2b 0 | 1 - results indicate "Block" does not significantly improve the model

# Simpler model without any random effects
lmer2d <- lm(Chlorosis ~ Treatment, data = chlorosis_scores)
summary(lmer2d)
anova(lmer2c,lmer2d)

#best fit model:
summary(lmer2d)

#None of the random effects seem to matter- doing a KW and dunn posthoc test for the chlorosis data
#Chlorosis
KW.test <- kruskal.test(Chlorosis ~ Treatment, data = chlorosis_scores)
dunn_res <- FSA::dunnTest(x = chlorosis_scores$Chlorosis,
                          g = chlorosis_scores$Treatment,
                          method = "bonferroni")
# Get p-values
dunn_pvals <- dunn_res$res

# Generate compact letter display
chlorosis_cld <- cldList(P.adj ~ Comparison,
                         data = dunn_pvals,
                         threshold = 0.05)

print(chlorosis_cld)
# Change EHA15.I to EHA105.I
leaf_damage_cld$Group[leaf_damage_cld$Group == "EHA15.I"] <- "EHA105.I"
print(leaf_damage_cld)
# Change EHA15.I to EHA105.I
chlorosis_cld$Group[chlorosis_cld$Group == "EHA15.I"] <- "EHA105.I"
print(chlorosis_cld)



