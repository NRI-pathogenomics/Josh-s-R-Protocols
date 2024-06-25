# aligned rank transform (ART)
### Step 1: Install and Load Necessary Packages
R
install.packages("ARTool")
install.packages("emmeans")
install.packages("multcompView") # for cld function
library(ARTool)
library(emmeans)
library(multcompView)
```
### Step 2: Create Example Data

set.seed(123)
continuous_var <- rnorm(100)
genotype <- factor(sample(c("Genotype1", "Genotype2", "Genotype3", "Genotype4", "Genotype5"), 100, replace = TRUE))
treatment <- factor(sample(c("Mock", "Inoculated"), 100, replace = TRUE))
data <- data.frame(continuous_var, genotype, treatment)
```
### Step 3: Perform Aligned Rank Transform

# Perform the aligned rank transform
art_model <- art(continuous_var ~ genotype * treatment, data = data)
anova(art_model)

# After step 3 eemeans will not work - try this modifacation (you will need to modify this for your data)
#Extract estimates 
table<-art_model$estimated.effects
# Extract aligned ranks
data$aligned_ranks <- art_model$residuals + table$`genotype:treatment`
# Fit a linear model on the aligned ranks
lm_model <- lm(data $aligned_ranks ~ genotype * treatment, data = data)
summary(lm_model)

# Calculate estimated marginal means and perform pairwise comparisons
emmeans_results <- emmeans(lm_model, ~ genotype * treatment)
pairwise_comparisons <- pairs(emmeans_results)
summary(pairwise_comparisons)
# Obtain compact letter display (cld) to group treatments
cld_results <- cld(emmeans_results, Letters = letters)
print(cld_results)

