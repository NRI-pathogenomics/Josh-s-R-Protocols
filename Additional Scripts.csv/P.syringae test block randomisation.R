# Load necessary packages
# install.packages("blocksdesign") # Uncomment if you need to install
library(blocksdesign)
setwd("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Additional Scripts.csv")
# Define treatments and replicate labels
treatments <- rep(c("mock", "inoculated"), each = 5)  # Two treatments, 5 replicates each
replicate_labels <- c(3, 5, 8, 9, 13, 15, 16, 22, 23, 24)  # Col-0 plant labels

# Combine treatments and labels into a data frame
design <- data.frame(
  Treatment = treatments,
  Col_0_label = replicate_labels
)

# Randomize the order within each block
set.seed(42)  # Set seed for reproducibility
design <- design[sample(nrow(design)), ]

# Add plot numbers for easy reference
design$Plot <- 1:nrow(design)

# Sort the randomized design by plot numbers and display it
design_ordered <- design[order(design$Plot), ]
print(design_ordered)

# Save the randomized block design to a CSV file
write.csv(design_ordered, "randomized_block_design_with_labels.csv", row.names = FALSE)
cat("Randomized block design with Col-0 labels saved as 'randomized_block_design_with_labels.csv'\n")
