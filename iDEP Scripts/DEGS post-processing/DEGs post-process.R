#DEGs post-process
library(dplyr)
library(VennDiagram)
#get the 0dpi degs file
degs_0 <- read.csv("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/iDEP Scripts/DEGS post-processing/0dpi_fold_change_values_DESeq2.csv")

#convert columns to numeric
degs_0 <- degs_0 %>%
  mutate(across(c(`WT_0dpi.X1703_0dpi_log2FC`, `WT_0dpi.X1703_0dpi_adjPval`), as.numeric))
# filter out any genes with an LFC that is -2 >= x >= 2

degs_0 <- degs_0[degs_0$WT_0dpi.X1703_0dpi_log2FC <= -2 | degs_0$WT_0dpi.X1703_0dpi_log2FC >= 2, ]

# filter out any genes with an adjPval >= 0.01

degs_0 <- degs_0[degs_0$`WT_0dpi.X1703_0dpi_adjPval` >= 0.01, ]

# filter out rows that have NAs - as any rows that have NAs in the seperate files are irrelevant
degs_0 <- na.omit(degs_0)

# add Up/down to the degs_df
x <- nrow(degs_0)
degs_0$`Up/Down` <- character(length = x)
for(i in 1:x){
  if(degs_0$`WT_0dpi.X1703_0dpi_log2FC`[i] <= 0) {
    degs_0$`Up/Down`[i] <- "Down"
  }
  if(degs_0$`WT_0dpi.X1703_0dpi_log2FC`[i] >= 0) {
    degs_0$`Up/Down`[i] <- "Up"
  }
}

#get the 2dpi degs file
degs_2 <- read.csv("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/iDEP Scripts/DEGS post-processing/2dpi_fold_change_values_DESeq2.csv")

#convert columns to numeric
degs_2 <- degs_2 %>%
  mutate(across(c(`WT_2dpi.X1703_2dpi_log2FC`, `WT_2dpi.X1703_2dpi_adjPval`), as.numeric))
# filter out any genes with an LFC that is -2 >= x >= 2

degs_2 <- degs_2[degs_2$WT_2dpi.X1703_2dpi_log2FC <= -2 | degs_2$WT_2dpi.X1703_2dpi_log2FC >= 2, ]

# filter out any genes with an adjPval >= 0.01

degs_2 <- degs_2[degs_2$`WT_2dpi.X1703_2dpi_adjPval` >= 0.01, ]

# filter out rows that have NAs - as any rows that have NAs in the seperate files are irrelevant
degs_2 <- na.omit(degs_2)
# add Up/down to the degs_df
x <- nrow(degs_2)
degs_2$`Up/Down` <- character(length = x)
for(i in 1:x){
  if(degs_2$`WT_2dpi.X1703_2dpi_log2FC`[i] <= 0) {
    degs_2$`Up/Down`[i] <- "Down"
  }
  if(degs_2$`WT_2dpi.X1703_2dpi_log2FC`[i] >= 0) {
    degs_2$`Up/Down`[i] <- "Up"
  }
}


# Venn diagrams
# Prepare a palette of 3 colors with R colorbrewer:
# Venn diagrams
# Prepare a palette of 3 colors with R colorbrewer:
library(RColorBrewer)
library(VennDiagram)
library(grid)

myCol <- brewer.pal(3, "Pastel2")

# Filter for upregulated and downregulated genes
upregulated_0 <- degs_0[degs_0$`Up/Down` == "Up", ]
downregulated_0 <- degs_0[degs_0$`Up/Down` == "Down", ]
upregulated_2 <- degs_2[degs_2$`Up/Down` == "Up", ]
downregulated_2 <- degs_2[degs_2$`Up/Down` == "Down", ]

# Extract Ensembl IDs
up_genes_0 <- upregulated_0$ensembl_ID
down_genes_0 <- downregulated_0$ensembl_ID
up_genes_2 <- upregulated_2$ensembl_ID
down_genes_2 <- downregulated_2$ensembl_ID

# Simple downregulated genes comparison
grid.newpage()  # Creates a new blank plotting page/canvas to draw on
draw.pairwise.venn(
  area1 = length(down_genes_0),  # Count total downregulated genes at 0 dpi (left circle size)
  area2 = length(down_genes_2),  # Count total downregulated genes at 2 dpi (right circle size)
  cross.area = length(intersect(down_genes_0, down_genes_2)),  # Count genes downregulated in BOTH conditions (overlap area)
  category = c("1703 vs WT Downregulated 0 dpi", "1703 vs WT Downregulated 2 dpi"),  # Text labels for each circle
  fill = myCol[1:2],  # Colors for the circles (first 2 colors from the Pastel2 palette)
  cat.pos = c(-20, 20),  # Position of category labels in degrees (-20° for left, +20° for right)
  cat.dist = 0.05  # Distance of category labels from the edge of circles (0.05 = close to circles)
)

# Simple upregulated genes comparison  
grid.newpage()  # Creates another new blank plotting page (clears previous plot)
draw.pairwise.venn(
  area1 = length(up_genes_0),  # Count total upregulated genes at 0 dpi (left circle size)
  area2 = length(up_genes_2),  # Count total upregulated genes at 2 dpi (right circle size)
  cross.area = length(intersect(up_genes_0, up_genes_2)),  # Count genes upregulated in BOTH conditions (overlap area)
  category = c("1703 vs WT Upregulated 0 dpi", "1703 vs WT Upregulated 2 dpi"),  # Text labels for each circle
  fill = myCol[1:2],  # Same colors as downregulated plot for consistency
  cat.pos = c(-20, 20),  # Same label positioning as downregulated plot
  cat.dist = 0.05  # Same label distance as downregulated plot
)