# Col-0 Batch 1 Mixed Model Analysis
# # Install and load required package
# if(!require(vcd)) install.packages("vcd")
# library(vcd)
# 
# # Calculate association measures
# assocstats(table(disease_scores$Treatment, disease_scores$Infiltrated.with.P.syringae))
# assocstats(table(disease_scores$Treatment, disease_scores$Fungus.gnats))
# assocstats(table(disease_scores$Treatment, disease_scores$Perforation))
install.packages("ordinal")
library(ordinal)
library(lme4)
# since the response variables are ordinal data
disease_scores <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 1/Batch 1 Scores.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")
# data processing
disease_scores <- na.omit(disease_scores) #removes NA values
disease_scores <- subset(disease_scores, select = -c(Chlorosis,Random, Block.Rep)) 
#removes chlorosis as this test is focusing on leaf damage
#randomized block design calculations and the block rep (not important for this model)

# #The model - models for individual factors (Perforation, P.syringae infiltration and fungus gnats) to determine which of the two is more closely associated with leaf damage
# 
# # Model with only P. syringae (hypothesis: P.syringae are more closely associated with damage)
# model_ps <- clm(as.ordered(Leaf.Damage) ~ Infiltrated.with.P.syringae, 
#                 data = disease_scores)
# 
# # Model with only fungus gnats (hypothesis: fungus gnats are more closely associated with damage)
# model_fg <- clm(as.ordered(Leaf.Damage) ~ Fungus.gnats, 
#                 data = disease_scores)
# 
# # Model with only Perforation (hypothesis: Infiltration process is closely associated with damage)
# model_pf <- clm(as.ordered(Leaf.Damage) ~ Perforation, 
#                 data = disease_scores)

#Complete Model
lmer1a <- lmer(Leaf.Damage ~ Treatment + Infiltrated.with.P.syringae + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation), data = disease_scores)
summary(lmer1a)

# Simpler Model (dropped Infiltrated.with.P.syringae)
lmer1b <- lmer(Leaf.Damage ~ Treatment + (1 | Block) + (1 | Fungus.gnats) + (1 | Perforation), data = disease_scores)
summary(lmer1b)

# Simpler Model (dropped Block as a Random effect)

lmer1c <- lmer(Leaf.Damage ~ Treatment + (1 | Fungus.gnats) + (1 | Perforation), data = disease_scores)
summary(lmer1c)

anova(lmer1a,lmer1b)
# compare whether meodle with block is significantly different to the model without - if it is then Block is important 
#It is in this case

lmer1d <- lmer(Leaf.Damage ~ Treatment + (1 | Block) + (1 | Perforation), data = disease_scores)
summary(lmer1d)
anova(lmer1b,lmer1d)
#Compare the model with Block and Fungus.gnats to the model with Block only
#P.value indicates Fungus gnats is importance
#Simpler model without the Perforation

lmer1e <- lmer(Leaf.Damage ~ Treatment + (1 | Block) + (1 | Fungus.gnats), data = disease_scores)
summary(lmer1e)
anova(lmer1b,lmer1e)
#Result indicates that perforation is not important - perforation does not explain a significant amount of variance in leaf damage

#Simplier model without Treatment

lmer1f <- lmer(Leaf.Damage ~ (1 | Block) + (1 | Fungus.gnats), data = disease_scores)
anova(lmer1e, lmer1f)

# Results indicate that treatment does explain a significant amount of variance in leaf damage

summary(lmer1e)

#No we need a post-hoc test to determine which treatments differ from one another:
# the magnitude of T-values indicates that at least one of the treatments differs from the others in terms of Leaf Damage but the post-hoc test will reveal which treatment differs
#In short: Treatment is a fixed effect and one or more treatment is behind the leaf damage
#Fungus.gnat damage is accounted for as a random effect that causes variance in the data
# but depending on the results of the post-hoc test, if one of the infected/agroinfiltrated treatments differs signficantly from the others then it/they are the reason for leaf degradation - ergo the stats will show if P.syringae presence is behind leaf degredation

# Null model (hypothesis: neither factor is more closely associated with damage)
model_null <- clm(as.ordered(Leaf.Damage) ~ 1, 
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
# 
