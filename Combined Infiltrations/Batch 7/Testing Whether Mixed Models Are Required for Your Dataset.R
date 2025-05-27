# Testing Whether Mixed Models Are Required for Your Dataset
library(lme4)
library(car)
library(nlme)

# Load your data (replace with your actual data loading)
# disease_scores <- read.csv("your_file.csv")
# ... your data processing steps ...

print("=== TESTING IF MIXED MODELS ARE REQUIRED ===")

# TEST 1: INTRACLASS CORRELATION COEFFICIENT (ICC)
# This tests if there's clustering/correlation within groups
print("\n1. INTRACLASS CORRELATION COEFFICIENT (ICC) TEST")
print("Tests if observations within groups are more similar than between groups")

# Check each potential grouping factor
grouping_factors <- c("Block", "Fungus.gnats", "Perforation")

for(factor in grouping_factors) {
  if(factor %in% names(disease_scores) && length(unique(disease_scores[[factor]])) > 1) {
    
    # Fit null model (intercept only) with random effect
    tryCatch({
      null_mixed <- lmer(PS..5.dpi.Leaf.Damage ~ 1 + (1|get(factor)), data = disease_scores)
      
      # Calculate ICC
      var_components <- as.data.frame(VarCorr(null_mixed))
      between_var <- var_components$vcov[1]  # Random effect variance
      within_var <- var_components$vcov[2]   # Residual variance
      icc <- between_var / (between_var + within_var)
      
      print(paste("ICC for", factor, ":", round(icc, 4)))
      
      if(icc > 0.05) {
        print(paste("  → SIGNIFICANT clustering in", factor, "- Mixed model recommended"))
      } else {
        print(paste("  → Low clustering in", factor, "- May not need random effect"))
      }
    }, error = function(e) {
      print(paste("Cannot calculate ICC for", factor, ":", e$message))
    })
  }
}

# TEST 2: LIKELIHOOD RATIO TEST
# Compare mixed model vs linear model
print("\n2. LIKELIHOOD RATIO TEST")
print("Compares mixed model to simple linear model")

# Fit linear model
lm_model <- lm(PS..5.dpi.Leaf.Damage ~ Treatment + Infiltrated.with.P.syringae, 
               data = disease_scores)

# Try to fit mixed model with available grouping factors
valid_random_effects <- c()
for(factor in grouping_factors) {
  if(factor %in% names(disease_scores) && length(unique(disease_scores[[factor]])) > 1) {
    valid_random_effects <- c(valid_random_effects, paste0("(1|", factor, ")"))
  }
}

if(length(valid_random_effects) > 0) {
  # Fit mixed model
  mixed_formula <- paste("PS..5.dpi.Leaf.Damage ~ Treatment + Infiltrated.with.P.syringae +", 
                         paste(valid_random_effects, collapse = " + "))
  
  tryCatch({
    mixed_model <- lmer(as.formula(mixed_formula), data = disease_scores)
    
    # Likelihood ratio test
    lr_test <- anova(lm_model, mixed_model)
    print("Likelihood Ratio Test Results:")
    print(lr_test)
    
    if(lr_test$`Pr(>Chisq)`[2] < 0.05) {
      print("  → SIGNIFICANT: Mixed model fits significantly better - USE MIXED MODEL")
    } else {
      print("  → NOT SIGNIFICANT: Linear model is adequate - Mixed model not required")
    }
  }, error = function(e) {
    print(paste("Could not fit mixed model:", e$message))
  })
} else {
  print("No valid random effects found - Mixed model cannot be fit")
}

# TEST 3: RESIDUAL ANALYSIS FOR INDEPENDENCE
print("\n3. RESIDUAL ANALYSIS FOR INDEPENDENCE")
print("Check if residuals show patterns within groups")

# Get residuals from linear model
residuals_lm <- residuals(lm_model)
disease_scores$residuals <- residuals_lm

# Check residuals within each grouping factor
for(factor in grouping_factors) {
  if(factor %in% names(disease_scores) && length(unique(disease_scores[[factor]])) > 1) {
    
    print(paste("\nChecking residuals within", factor, ":"))
    
    # Calculate correlation of residuals within groups
    group_cors <- by(disease_scores$residuals, disease_scores[[factor]], 
                     function(x) if(length(x) > 1) cor(x[-1], x[-length(x)]) else NA)
    
    avg_cor <- mean(unlist(group_cors), na.rm = TRUE)
    print(paste("Average within-group correlation:", round(avg_cor, 4)))
    
    if(abs(avg_cor) > 0.1) {
      print(paste("  → Residuals are correlated within", factor, "- Mixed model recommended"))
    } else {
      print(paste("  → Low correlation within", factor, "- Independence assumption may hold"))
    }
  }
}

# TEST 4: VARIANCE HETEROGENEITY TEST
print("\n4. VARIANCE HETEROGENEITY TEST")
print("Test if different groups have different variances")

for(factor in grouping_factors) {
  if(factor %in% names(disease_scores) && length(unique(disease_scores[[factor]])) > 1) {
    
    # Levene's test for homogeneity of variance
    tryCatch({
      levene_test <- leveneTest(PS..5.dpi.Leaf.Damage ~ get(factor), data = disease_scores)
      print(paste("\nLevene's test for", factor, ":"))
      print(paste("F =", round(levene_test$`F value`[1], 4), 
                  ", p =", round(levene_test$`Pr(>F)`[1], 4)))
      
      if(levene_test$`Pr(>F)`[1] < 0.05) {
        print(paste("  → SIGNIFICANT: Variances differ across", factor, "- Consider random effects"))
      } else {
        print(paste("  → NOT SIGNIFICANT: Variances are homogeneous for", factor))
      }
    }, error = function(e) {
      print(paste("Could not perform Levene's test for", factor))
    })
  }
}

# TEST 5: DESIGN-BASED ASSESSMENT
print("\n5. DESIGN-BASED ASSESSMENT")
print("Based on your experimental design structure:")

# Check data structure
n_total <- nrow(disease_scores)
n_treatments <- length(unique(disease_scores$Treatment))
n_per_treatment <- table(disease_scores$Treatment)

print(paste("Total observations:", n_total))
print(paste("Number of treatments:", n_treatments))
print("Observations per treatment:")
print(n_per_treatment)

# Check for potential clustering
for(factor in grouping_factors) {
  if(factor %in% names(disease_scores)) {
    n_groups <- length(unique(disease_scores[[factor]]))
    if(n_groups > 1) {
      obs_per_group <- table(disease_scores[[factor]])
      avg_obs_per_group <- mean(obs_per_group)
      
      print(paste("\n", factor, ":"))
      print(paste("  Number of groups:", n_groups))
      print(paste("  Average observations per group:", round(avg_obs_per_group, 1)))
      
      if(avg_obs_per_group > 1.5 && n_groups > 2) {
        print(paste("  → Multiple observations per group - Mixed model likely needed"))
      } else {
        print(paste("  → Few observations per group - May not need random effect"))
      }
    }
  }
}

# FINAL RECOMMENDATION
print("\n=== FINAL RECOMMENDATION ===")
print("Consider the results above:")
print("- ICC > 0.05: Suggests clustering, use mixed model")
print("- Significant LR test: Mixed model fits better")
print("- Correlated residuals within groups: Use mixed model") 
print("- Heterogeneous variances: Consider mixed model")
print("- Multiple obs per group in design: Likely need mixed model")