# cld_AUDPC Script
# Redoes the AUDPC plot with the clustered letter display data
# create a new cld_df with the dataframe in the same order they are in the combobarplot_long df
melt_df <- cld_df[order(cld_df$Group), ]

# Merge Standard Error data back into combo_barplotdata_long dataset
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