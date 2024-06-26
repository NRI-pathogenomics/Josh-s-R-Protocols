# aligned rank transform (ART)
### Step 1: Install and Load Necessary Packages
if(("ARTool" %in% all_packages)==FALSE){
  install.packages("ARTool")}
if(("emmeans" %in% all_packages)==FALSE){
  install.packages("emmeans")}
if(("multcompView" %in% all_packages)==FALSE){
  install.packages("multcompView") # for cld function
  }
library(ARTool)
library(emmeans)
library(multcompView)

### Step 2: Create Example Data


data <- Results %>% select(Result_Genotype,Result_Type, Result_AUDPC)

rownames(data) <- NULL

### Step 3: Perform Aligned Rank Transform

# Perform the aligned rank transform
art_model <- art(Result_AUDPC ~ Result_Genotype * Result_Type, data = data)
anova(art_model)

# After step 3 eemeans will not work - try this modifacation (you will need to modify this for your data)
#Extract estimates 
table<-art_model$estimated.effects
# Extract aligned ranks
data$aligned_ranks <- art_model$residuals + table$`Result_Genotype:Result_Type`
# Fit a linear model on the aligned ranks
lm_model <- lm(data $aligned_ranks ~ Result_Genotype * Result_Type, data = data)
summary(lm_model)

# Calculate estimated marginal means and perform pairwise comparisons
emmeans_results <- emmeans(lm_model, ~ Result_Genotype * Result_Type)
pairwise_comparisons <- pairs(emmeans_results)
summary(pairwise_comparisons)
# Obtain compact letter display (cld) to group Result_Types
cld_results <- cld(emmeans_results, Letters = letters)
print(cld_results)

