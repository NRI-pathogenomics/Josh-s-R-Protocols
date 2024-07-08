#Kruskal-Wallis Test
# install.packages("FSA") # Houses dunnTest for pair wise comparison
# install.packages("ggpubr")  # For density plot and for creating and customizing 'ggplot2'- based publication ready plots
# install.packages("ggstatplot") # Houses gbetweenstats() function that allows building a combination of box and violin plots along with                                        statistical details.
# install.packages("tidyverse") # For wrangling and tidying the data
# install.packages("MultNonParam")
# install.packages("FSA")      # Houses dunnTest for pairwise comparison
# install.packages("ggpubr")   # For density plot and for creating and customizing 'ggplot2' based publication-ready plots
# install.packages("tidyverse")# For wrangling and tidying the data
# install.packages("dunn.test")
# install.packages("rcompanion")
library(MASS)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggpmisc)
library(jmv)
library(Rmisc)
library(emmeans) 
library(multcomp)
library(Hmisc)
library(lattice)
library(multcompView)
library(agricolae)
library(bestNormalize)
library(MultNonParam)
library(dunn.test)
library(FSA)
library(tidyverse)
library(ggpubr)

data <- Results %>% select(Result_Genotype,Result_Type, Result_AUDPC)

rownames(data) <- NULL

#convert rows and values to required format
data$Result_Genotype <- gsub("-", "_", data$Result_Genotype)
data$Result_AUDPC <- as.numeric(data$Result_AUDPC)
data$Result_Genotype <- as.factor(unlist(data$Result_Genotype))
data$Result_Type <- as.factor(unlist(data$Result_Type))

### Step 3: Perform Kruskal-Wallis Test
#On the data overall
print("Mock vs Inoculated")
KW_results1 <- kruskal.test(Result_AUDPC, formula=Result_AUDPC ~ Result_Genotype * Result_Type, data = data)
print(KW_results1)

# Kruskal-Wallis Test
print("Mock vs Inoculated")
KW_results1 <- kruskal.test(Result_AUDPC, Result_AUDPC ~ Result_Genotype * Result_Type, data = data)
print(KW_results1)

# Post Hoc: Perform Dunn's Test
dunn_result <- dunn.test(data$Result_AUDPC, g=interaction(data$Result_Genotype, data$Result_Type), method="bonferroni")
print("Post-Hoc Dunn Test")
print(dunn_result)

# Load the necessary library to perform a cld test on this dunn.test output
library(rcompanion)

# Create a compact letter display (cld)
cld_result <- cldList(data=dunn_result, 
                      formula =  P ~ comparisons, 
                      comparison=comparisons, 
                      threshold = 0.05, 
                      print.comp = TRUE)
print(cld_result)
# Convert CLD results to a data frame (ensures it's in the correct format)
cld_df <- as.data.frame(cld_result)

# # Ensure that the grouping factor is ordered correctly
# cld_df$Result_Genotype <- factor(cld_df$Result_Genotype, levels = unique(cld_df$Result_Genotype))
# cld_df$Result_Type <- factor(cld_df$Result_Type, levels = unique(cld_df$Result_Type))
# # this groups the results in the desired manner and only selects the unique values for each treatment type and genotype
# 
# # Rename columns for clarity (optional)
# colnames(cld_df)[colnames(cld_df) == ".group"] <- "CLD_Group"
# 
# # Create the bar plot
# KW_plot <- ggplot(cld_df, aes(x = Result_Type, y = emmean, fill = Result_Genotype)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
#   geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
#                 position = position_dodge(width = 0.8), width = 0.25) +
#   geom_text(aes(label = CLD_Group, y = emmean + SE + 0.1),
#             position = position_dodge(width = 0.8), vjust = 0) +
#   labs(x = "Result Type", y = "Estimated Marginal Means", fill = "Result Genotype",
#        title = "Bar Plot with Compact Letter Display") +
#   theme_minimal()
# print(art_plot)
