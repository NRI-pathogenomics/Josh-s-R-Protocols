# Define treatments and replicate labels for a single block
setwd("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Additional Scripts.csv")
# Define treatments and specific replicate labels for each
treatment_labels <- data.frame(
  Treatment = c(rep("inoculated", 5), rep("mock", 5)),
  Col_0_label = c(3, 5, 8, 9, 13, 15, 16, 22, 23, 24)
)

# Create two randomized blocks using the specific labels for each treatment
set.seed(42)  # Set seed for reproducibility
block1 <- treatment_labels[sample(nrow(treatment_labels)), ]  # Randomize rows for Block 1
block2 <- treatment_labels[sample(nrow(treatment_labels)), ]  # Randomize rows for Block 2

# Add Block identifiers
block1$Block <- 1
block2$Block <- 2

# Combine both blocks into one data frame
design <- rbind(block1, block2)

# Add Plot numbers within each block for easy reference
design <- design[order(design$Block), ]
design$Plot <- unlist(lapply(split(design, design$Block), function(df) 1:nrow(df)))

# Display the randomized block design
print(design)

# Save the randomized block design to a CSV file
write.csv(design, "randomized_block_design_with_two_blocks.csv", row.names = FALSE)
cat("Randomized block design with two blocks saved as 'randomized_block_design_with_two_blocks.csv'\n")
