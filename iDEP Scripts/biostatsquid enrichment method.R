#biostatssquid enrichment plot method
# For data management
# install.packages('tidyverse')
# BiocManager::install("clusterProfiler")
# BiocManager::install("org.Hs.eg.db")
# # For visualisation
# install.packages('pheatmap')
# install.packages("DOSE")
# install.packages("enrichplot")
# install.packages("ggupset")
# Load required libraries
library(tidyverse)
library(clusterProfiler)

# Read the CSV
enrichres_df <- read.csv("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/iDEP Scripts/1703 0dpi-2dpi down.csv")

# Format the enrichment results
enrichres_formatted <- enrichres_df %>%
  mutate(
    ID = paste0("PATH:", row_number()),
    Description = Pathway,
    GeneRatio = paste0(nGenes, "/", nGenes),
    BgRatio = paste0(`Pathway.size`, "/10000"),
    pvalue = FDR,
    p.adjust = FDR,
    qvalue = FDR,
    geneID = "",
    Count = nGenes,
    FoldChange = Fold.enriched
  ) %>%
  select(ID, Description, GeneRatio, BgRatio, pvalue, p.adjust, qvalue, geneID, Count, FoldChange)

# Create a data frame for ggplot
plot_df <- enrichres_formatted %>%
  mutate(qscore = -log10(p.adjust)) %>%
  arrange(desc(qscore)) %>%
  slice_head(n = 20)  # Show top 20 pathways

# Plot using ggplot2
ggplot(plot_df, aes(x = reorder(Description, qscore), y = qscore, fill = FoldChange)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    x = NULL,
    y = expression(-log[10](FDR)),
    fill = "Fold Change") +
  theme_minimal(base_size = 14)
