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
                      print.comp = TRUE, remove.zero = FALSE)
#Remove.zero is important as it prevents the Col0 group from becoming Col allowing for a normal left_join to occur
print(cld_result)
# Convert CLD results to a data frame (ensures it's in the correct format)
cld_df <- as.data.frame(cld_result)

#Add the Cld results to the "data" dataframe
#First, some reshaping of the data dataframe
data$Group <- paste(data$Result_Genotype, data$Result_Type, sep=".")

# now left join the cld_result to the data data_frame
data <- data %>% left_join(cld_result,by="Group")
print(data)

