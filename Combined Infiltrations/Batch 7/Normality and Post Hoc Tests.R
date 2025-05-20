#Col-0 Batch 7 Data modelling

library(ordinal)
library(multcompView)
library(rcompanion)
library(ordinal)
library(lme4)
# since the response variables are ordinal data
disease_scores <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 7/Batch 7 scores.csv", 
                           header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "")
# data processing
damage_scores <- subset(disease_scores, select = -c(Random, Block.Rep, PS..0.dpi.Leaf.Damage, PS..0.dpi.Chlorosis,
                                                    PS..5.dpi.Leaf.Damage,
                                                    PS..5.dpi.Chlorosis,
                                                    Infiltrated.with.P.syringae, Fungus.gnats, Perforation))
damage_scores$Treatment <- gsub("-", ".", damage_scores$Treatment)
damage_scores <- na.omit(damage_scores) #removes NA values
#removes Agro..5.dpi.Chlorosis as this test is focusing on leaf damage

## Batches Normality Test
Batch_7_LD <- shapiro.test(damage_scores$Agro..5.dpi.Leaf.Damage)
Batch_7_CL <- shapiro.test(Path_Assay$Agro..5.dpi.Chlorosis)

#results
if(as.numeric(Batch_7_LD$p.value) > 0.05){
  print("Leaf Damage Scores are normally distributed and requires a parametric test")
}else{
  print("Leaf Damage Scores are NOT normally Distributed and requires a non-parametric test")}
if(as.numeric(Batch_7_CL$p.value) > 0.05){
  print("Chlorosis Scores are normally distributed and requires a parametric test")
}else{
  print("Chlorosis Scores are NOT normally Distributed and requires a non-parametric test")}

# non-parametric test (Kruskal Wallis and Dunn test)
#Leaf Damage
KW.test <- kruskal.test(Agro..5.dpi.Leaf.Damage ~ Treatment, data = damage_scores)
dunn_res <- FSA::dunnTest(x = damage_scores$Agro..5.dpi.Leaf.Damage,
          g = damage_scores$Treatment,
          method = "bonferroni")
# Get p-values
dunn_pvals <- dunn_res$res

# Generate compact letter display
leaf_damage_cld <- cldList(P.adj ~ Comparison,
               data = dunn_pvals,
               threshold = 0.05)

#Chlorosis
KW.test <- kruskal.test(Agro..5.dpi.Chlorosis ~ Treatment, data = damage_scores)
dunn_res <- FSA::dunnTest(x = damage_scores$Agro..5.dpi.Chlorosis,
                          g = damage_scores$Treatment,
                          method = "bonferroni")
# Get p-values
dunn_pvals <- dunn_res$res

# Generate compact letter display
chlorosis_cld <- cldList(P.adj ~ Comparison,
                           data = dunn_pvals,
                           threshold = 0.05)

print(chlorosis_cld)
# Change EHA15.I to EHA105.I
leaf_damage_cld$Group[disease_treatments_cld$Group == "EHA15.I"] <- "EHA105.I"
print(leaf_damage_cld)
# Change EHA15.I to EHA105.I
chlorosis_cld$Group[disease_treatments_cld$Group == "EHA15.I"] <- "EHA105.I"
print(chlorosis_cld)