# aligned rank transform (ART)
### Step 1: Install and Load Necessary Packages
```R
install.packages("ARTool")
install.packages("emmeans")
install.packages("multcompView") # for cld function
library(ARTool)
library(emmeans)
library(multcompView)
```
### Step 2: Create Example Data
```R
set.seed(123)
continuous_var <- rnorm(100)
genotype <- factor(sample(c("Genotype1", "Genotype2", "Genotype3", "Genotype4", "Genotype5"), 100, replace = TRUE))
treatment <- factor(sample(c("Mock", "Inoculated"), 100, replace = TRUE))
data <- data.frame(continuous_var, genotype, treatment)
```
### Step 3: Perform Aligned Rank Transform
```R
# Perform the aligned rank transform
art_model <- art(continuous_var ~ genotype * treatment, data = data)
anova(art_model)
```
# After step 3 eemeans will not work - try this modifacation (you will need to modify this for your data)
#Extract estimates 
table<-art_model$estimated.effects
# Extract aligned ranks
dataR$aligned_ranks <- art_model$residuals + table$`data$genotype:data$treatment`
# Fit a linear model on the aligned ranks
lm_model <- lm(dataR$aligned_ranks ~ genotype * treatment, data = data)
summary(lm_model)
# Calculate estimated marginal means and perform pairwise comparisons
emmeans_results <- emmeans(lm_model, ~ genotype * treatment)
pairwise_comparisons <- pairs(emmeans_results)
summary(pairwise_comparisons)
# Obtain compact letter display (cld) to group treatments
cld_results <- cld(emmeans_results, Letters = letters)
print(cld_results)
### Step 4: Post-Hoc Test with Pairwise Comparisons
```R
# Perform pairwise comparisons
em <- emmeans(art_model, ~ genotype * treatment)
pairwise <- pairs(em)
# Summarize pairwise comparisons
summary(pairwise)
```
### Step 5: Use Compact Letter Display for Significance Grouping
```R
# Obtain compact letter display (cld) to group treatments
cld <- cld(em, Letters = letters)
print(cld)
```
### Full Example in R
Here’s the full example combined:
  ```R
# Install and load packages
install.packages("ARTool")
install.packages("emmeans")
install.packages("multcompView")
library(ARTool)
library(emmeans)
library(multcompView)
# Create example data
set.seed(123)
continuous_var <- rnorm(100)
genotype <- factor(sample(c("Genotype1", "Genotype2", "Genotype3", "Genotype4", "Genotype5"), 100, replace = TRUE))
treatment <- factor(sample(c("Mock", "Inoculated"), 100, replace = TRUE))
data <- data.frame(continuous_var, genotype, treatment)
# Perform the aligned rank transform
art_model <- art(continuous_var ~ genotype * treatment, data = data)
anova(art_model)
# Perform pairwise comparisons
em <- emmeans(art_model, ~ genotype * treatment)
pairwise <- pairs(em)
# Summarize pairwise comparisons
summary(pairwise)
# Obtain compact letter display (cld) to group treatments
cld <- cld(em, Letters = letters)
print(cld)
```
### Explanation
# 1. **Data Preparation**: Create a data frame with your continuous variable and the two categorical variables (genotype and treatment).
# 2. **Aligned Rank Transform**: The `art` function from the `ARTool` package is used to perform the aligned rank transform on the data.
# 3. **ANOVA**: Perform an ANOVA on the transformed data to analyze the interaction between the categorical variables and the continuous variable.
# 4. **Pairwise Comparisons**: Use `emmeans` to compute estimated marginal means and perform pairwise comparisons.
# 5. **Compact Letter Display**: The `cld` function from the `multcompView` package is used to generate compact letter displays, which indicate groups of treatments that are not significantly different from each other.
# This approach allows you to perform non-parametric analysis on your data and interpret the results with clear groupings of treatments based on significance.