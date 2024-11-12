# Define treatments and replicate labels for a single block
treatments <- rep(c("mock", "inoculated"), each = 5)  # Two treatments, 5 replicates each
replicate_labels <- c(3, 5, 8, 9, 13, 15, 16, 22, 23, 24)  # Col-0 plant labels

# Create a single block with treatments and replicate labels
single_block <- data.frame(
  Treatment = treatments,
  Col_0_label = replicate_labels
)

# Duplicate the single block to create two blocks
block1 <- single_block
block2 <- single_block

# Randomize the rows within each block
set.seed(42)  # Set seed for reproducibility
block1 <- block1[sample(nrow(block1)), ]
block2 <- block2[sample(nrow(block2)), ]

# Add Block identifiers
block1$Block <- 1
block2$Block <- 2

# Combine both blocks into one data frame
design <- rbind(block1, block2)

# Add Plot numbers within each block
design <- design[order(design$Block), ]
design$Plot <- unlist(lapply(split(design, design$Block), function(df) 1:nrow(df)))

# Display the randomized block design
print(design)

# Save the randomized block design to a CSV file
write.csv(design, "randomized_block_design_with_two_blocks.csv", row.names = FALSE)
cat("Randomized block design with two blocks saved as 'randomized_block_design_with_two_blocks.csv'\n")
