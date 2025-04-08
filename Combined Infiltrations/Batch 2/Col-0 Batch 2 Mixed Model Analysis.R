# Col-0 Batch 2 Mixed Model Analysis
install.packages("ordinal")
library(ordinal)
# since the response variables are ordinal data
disease_scores <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 2/Batch 2 Scores.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")
# data processing
disease_scores <- na.omit(disease_scores) #removes NA values
disease_scores <- subset(disease_scores, select = -c(Chlorosis,Random, Block.Rep)) 
disease_scores <- subset(disease_scores, Perforation == "Y")
#removes chlorosis as this test is focusing on leaf damage
#randomized block design calculations and the block rep (not important for this model)

#The model - models for individual factor (P.syringae infiltration and fungus gnats) to determine which of the two is more closely associated with leaf damage

# Model with only P. syringae (hypothesis: P.syringae are more closely associated with damage)
model_ps <- clm(as.ordered(Leaf.Damage) ~ Infiltrated.with.P.syringae, 
                data = disease_scores)

# Model with only fungus gnats (hypothesis: fungus gnats are more closely associated with damage)
model_fg <- clm(as.ordered(Leaf.Damage) ~ Fungus.gnats, 
                data = disease_scores)

# Null model (hypothesis: neither factor is more closely associated with damage)
model_null <- clm(as.ordered(Leaf.Damage) ~ 1, 
                  data = disease_scores)

# Compare using likelihood ratio tests using a default P.value of < 0.05
anova(model_null, model_ps)
anova(model_null, model_fg)

#summary to quantify the effect size of each factor
summary(model_ps)
summary(model_fg)
