#Col-0 Batch 2 Data modelling

library(ordinal)
library(multcompView)
library(rcompanion)
library(ordinal)
library(lme4)
# since the response variables are ordinal data
disease_scores <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 2/Batch 2 rescores.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")

# Data processing - select relevant columns based on your dataset structure
damage_scores <- subset(disease_scores, select = c(Col.0.replicate, Treatment, Block, 
                                                   X0.dpi.Leaf.Damage, X0.dpi.Chlorosis,
                                                   X5.dpi.Leaf.Damage, X5.dpi.Chlorosis))

# Clean treatment names (replace hyphens with dots if needed)
damage_scores$Treatment <- gsub("-", ".", damage_scores$Treatment)
damage_scores <- na.omit(damage_scores) # removes NA values

# Convert damage scores to factors for ordinal analysis if needed
damage_scores$X5.dpi.Leaf.Damage <- as.numeric(damage_scores$X5.dpi.Leaf.Damage)
damage_scores$X5.dpi.Chlorosis <- as.numeric(damage_scores$X5.dpi.Chlorosis)

## Normality Tests
Batch_2_LD <- shapiro.test(damage_scores$X5.dpi.Leaf.Damage)
Batch_2_CL <- shapiro.test(damage_scores$X5.dpi.Chlorosis)

# Results
if(as.numeric(Batch_2_LD$p.value) > 0.05){
  print("Leaf Damage Scores are normally distributed and requires a parametric test")
}else{
  print("Leaf Damage Scores are NOT normally distributed and requires a non-parametric test")
}

if(as.numeric(Batch_2_CL$p.value) > 0.05){
  print("Chlorosis Scores are normally distributed and requires a parametric test")
}else{
  print("Chlorosis Scores are NOT normally distributed and requires a non-parametric test")
}

# Non-parametric test (Kruskal Wallis and Dunn test)
# Leaf Damage Analysis
print("=== LEAF DAMAGE ANALYSIS ===")
KW.test.LD <- kruskal.test(X5.dpi.Leaf.Damage ~ Treatment, data = damage_scores)
print(KW.test.LD)

dunn_res_LD <- FSA::dunnTest(x = damage_scores$X5.dpi.Leaf.Damage,
                             g = damage_scores$Treatment,
                             method = "bonferroni")

# Get p-values
dunn_pvals_LD <- dunn_res_LD$res

# Generate compact letter display
leaf_damage_cld <- cldList(P.adj ~ Comparison,
                           data = dunn_pvals_LD,
                           threshold = 0.05)

print("Leaf Damage Compact Letter Display:")
print(leaf_damage_cld)

# Chlorosis Analysis
print("=== CHLOROSIS ANALYSIS ===")
KW.test.CL <- kruskal.test(X5.dpi.Chlorosis ~ Treatment, data = damage_scores)
print(KW.test.CL)

dunn_res_CL <- FSA::dunnTest(x = damage_scores$X5.dpi.Chlorosis,
                             g = damage_scores$Treatment,
                             method = "bonferroni")

# Get p-values
dunn_pvals_CL <- dunn_res_CL$res

# Generate compact letter display
chlorosis_cld <- cldList(P.adj ~ Comparison,
                         data = dunn_pvals_CL,
                         threshold = 0.05)

print("Chlorosis Compact Letter Display:")
print(chlorosis_cld)
