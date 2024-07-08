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

# Load libraries
library(FSA)
library(tidyverse)
library(ggpubr)

# Assuming 'Results' is already loaded
data <- Results %>% select(Result_Genotype, Result_Type, Result_AUDPC)
rownames(data) <- NULL

# Convert rows to required format
data$Result_AUDPC <- as.numeric(data$Result_AUDPC)
data$Result_Genotype <- as.factor(unlist(data$Result_Genotype))
data$Result_Type <- as.factor(unlist(data$Result_Type))

# Kruskal-Wallis Test
print("Mock vs Inoculated")
KW_results1 <- kruskal.test(Result_AUDPC, Result_AUDPC ~ Result_Genotype * Result_Type, data = data)
print(KW_results1)

# Post Hoc: Perform Dunn's Test
dunn_result <- dunn.test(data$Result_AUDPC, g=interaction(data$Result_Genotype, data$Result_Type), method="bonferroni")
print(dunn_result)

# Load the necessary library to perform a cld test on this dunn.test output
library(rcompanion)

# Create a compact letter display (cld)
cld_result <- cldList(data=dunn_result, 
                      formula = P.adjusted ~ comparisons, 
                      comparison=comparisons, 
                      threshold = 0.05, 
                      print.comp = TRUE)
print(cld_result)


# # Merge CLD results with data for plotting
# data <- data %>%
#   mutate(group = interaction(Result_Genotype, Result_Type)) %>%
#   left_join(cld_results, by = c("group" = "Group"))
# 
# print(cld_results)
# 
# # Plot results with ggplot2
# p <- ggplot(data, aes(x = interaction(Result_Genotype, Result_Type), y = Result_AUDPC)) +
#   geom_violin(trim = FALSE, fill = "lightblue") +
#   geom_boxplot(width = 0.1, fill = "orange", outlier.shape = NA) +
#   stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
#   geom_text(aes(label = Letters), position = position_dodge(width = 0.8), vjust = -0.5) +
#   theme_minimal() +
#   labs(title = "AUDPC Results by Genotype and Type",
#        x = "Genotype and Type",
#        y = "AUDPC")
# 
# # Display the plot
# print(p)