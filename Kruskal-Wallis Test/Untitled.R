#Kruskal-Wallis Test
install.packages("FSA") # Houses dunnTest for pair wise comparison
install.packages("ggpubr")  # For density plot and for creating and customizing 'ggplot2'- based publication ready plots
install.packages("ggstatplot") # Houses gbetweenstats() function that allows building a combination of box and violin plots along with                                        statistical details.
install.packages("tidyverse") # For wrangling and tidying the data
install.packages("MultNonParam")
install.packages("FSA")      # Houses dunnTest for pairwise comparison
install.packages("ggpubr")   # For density plot and for creating and customizing 'ggplot2' based publication-ready plots
install.packages("tidyverse")# For wrangling and tidying the data

library(MultNonParam)
data <- Results %>% select(Result_Genotype,Result_Type, Result_AUDPC)

rownames(data) <- NULL

#convert rows to required format

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
KW_results1 <- kruskal.test(Result_AUDPC ~ Result_Genotype * Result_Type, data = data)
print(KW_results1)

# Post-hoc Test: Perform Dunn's Test
dunn_results <- dunnTest(Result_AUDPC ~ Result_Genotype * Result_Type, data = data, method = "bonferroni")
print(dunn_results)

# Summarize data for plotting
summary_data <- data %>%
  group_by(Result_Genotype, Result_Type) %>%
  summarise(mean_AUDPC = mean(Result_AUDPC, na.rm = TRUE),
            sd_AUDPC = sd(Result_AUDPC, na.rm = TRUE))

# Plot results with ggplot2
p <- ggplot(data, aes(x = interaction(Result_Genotype, Result_Type), y = Result_AUDPC)) +
  geom_violin(trim = FALSE, fill = "lightblue") +
  geom_boxplot(width = 0.1, fill = "orange", outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  theme_minimal() +
  labs(title = "AUDPC Results by Genotype and Type",
       x = "Genotype and Type",
       y = "AUDPC")

# Display the plot
print(p)
