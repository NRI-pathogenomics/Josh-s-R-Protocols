# cld_AUDPC Script
# Redoes the AUDPC plot with the clustered letter display data
library(dplyr)
# create a new cld_df with the dataframe in the same order they are in the combobarplot_long df
melt_df <- cld_df %>% 
  arrange(desc(grepl("\\.M$", Group)), Group)

combo_barplotdata <- rbind(Mock_column, Inno_column)


# When it comes to the Standard Error I need to filter through the individual AUDPC replicates data to get the number of observations per per genotype
# add a number of replicates per treatment to the data set

# Calculate Standard Error:

standard_error_data <- list()

# Reshape data into long format
combo_barplotdata_long <- melt(combo_barplotdata, id.vars = "Genotype + Treatment")
combo_barplotdata$`Genotype + Treatment` <- as.factor(as.character(combo_barplotdata$`Genotype + Treatment`))

# Check the structure of Genotypes
str(combo_barplotdata_long$`Genotype + Treatment`)
# numerical data 
combo_barplotdata_long$value <- as.numeric(combo_barplotdata_long$value)

# Check the structure of Genotypes
str(combo_barplotdata_long$`Genotype + Treatment`)
# convert value to numerical data 
combo_barplotdata_long$value <- as.numeric(combo_barplotdata_long$value)

#use the loop reliably calculate the SE value for each treatment using the INDVIDUAL AUDPC data

# In the loop create a subset of the Results data frame, first for the Mock then for the Innoculated

standard_error_data <- list()
mock_se <- list()
inno_se <- list()
for(i in 1:GenoNumber){
  #Mock
  subset2 <- subset(Results, Result_Genotype == Genotypes[i])
  subset2 <- subset(subset2, Result_Type == "M")
  print(subset2)
  mock.st.dev <- sd(as.numeric(subset2$Result_AUDPC))
  print(paste("Standard Deviation: ", mock.st.dev))
  mock.no.of.reps <- length(subset2$Result_Type) 
  mock.no.of.reps <- mock.no.of.reps + 1
  print(paste("number of reps: ", mock.no.of.reps))
  mock.SE.value <- mock.st.dev/sqrt(mock.no.of.reps)
  print(paste("SE value ",mock.SE.value))
  mock_se <- append(mock_se, mock.SE.value)
  #Inocculated
  subset3 <- subset(Results, Result_Genotype == Genotypes[i])
  subset3 <- subset(subset3, Result_Type == "I")
  print(subset3)
  inno.st.dev <- sd(as.numeric(subset3$Result_AUDPC))
  print(paste("Standard Deviation: ", inno.st.dev))
  inno.no.of.reps <- length(subset3)
  inno.no.of.reps <- inno.no.of.reps + 1
  print(paste("number of reps: ", inno.no.of.reps))
  print(inno.no.of.reps)
  inno.SE.value <- inno.st.dev/sqrt(inno.no.of.reps)
  print(paste("SE value ",inno.SE.value))
  inno_se <- append(inno_se, inno.SE.value)
}
standard_error_data <- append(mock_se, inno_se)
## Corrected Bar Plot

# melt function from reshape2 package converts the data from a wide to a long format (A long format contains values that do repeat in the first column)
# Plot the side-by-side stacked bar plot

## Corrected Bar Plot

# melt function from reshape2 package converts the data from a wide to a long format (A long format contains values that do repeat in the first column)
# Plot the side-by-side stacked bar plot

# Merge Standard Error data back into combo_barplotdata_long dataset
combo_barplotdata_long$SE <- standard_error_data

#convert SE to numeric data

combo_barplotdata_long$SE <- as.numeric(combo_barplotdata_long$SE)

#add cld values to the combobarplotdata_long df

combo_barplotdata_long$cld <- melt_df$MonoLetter

# Get unique levels of Treatment
treatment_levels <- unique(combo_barplotdata_long$`Genotype + Treatment`)

# Generate colors for each unique level of Treatment
colors <- rainbow(length(treatment_levels))

# Map colors to Treatment levels
color_mapping <- setNames(colors, treatment_levels)

# Plot with updated scale_fill_manual
# Plot with updated theme to remove x-axis labels
AUDPC_Plot <- ggplot(data = combo_barplotdata_long, aes(x = `Genotype + Treatment`, y = value, fill = `Genotype + Treatment`)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  labs(title = "AUDPC of Mock vs Inoculated",
       x = "Genotypes", y = "Average AUDPC", fill = "Treatment") +
  scale_fill_manual(values = color_mapping) +  # Use color_mapping
  geom_errorbar(aes(ymin = value - SE, ymax = value + SE), position = position_dodge(width = 1), 
                width = 0.2, colour = "black", alpha = 0.5, linewidth = 0.5) +
  theme(axis.text.x = element_blank())  # Remove x-axis labels

show(AUDPC_Plot)

#geomerrorbar() needs to be used but it needs to be able to work on both bars