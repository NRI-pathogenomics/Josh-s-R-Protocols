#biostatssquid enrichment plot method
# For data management
install.packages('tidyverse')
BiocManager::install("clusterProfiler")
BiocManager::install("org.Hs.eg.db")
# For visualisation
install.packages('pheatmap')
install.packages("DOSE")
install.packages("enrichplot")
install.packages("ggupset")


# Load required libraries
library(tidyverse)
library(enrichplot)
library(clusterProfiler)
library(DOSE)

# Interactive file selection
enrichres_df <- read.csv("/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/iDEP Scripts/1703 0dpi-2dpi down.csv")

# Create enrichResult object from your CSV data
enrichres_formatted <- enrichres_df %>%
  mutate(
    ID = paste0("PATH:", row_number()),  # Create unique IDs
    Description = Pathway,               # Use your Pathway column
    GeneRatio = paste0(nGenes, "/", nGenes),  # Format as ratio
    BgRatio = paste0(`Pathway.size`, "/10000"),  # Background ratio (approximate)
    pvalue = FDR,                        # Use FDR as p-value
    p.adjust = FDR,                      # Adjusted p-value
    qvalue = FDR,                        # q-value
    geneID = "",                         # Empty for pathway analysis
    Count = nGenes                       # Gene count
  ) %>%
  select(ID, Description, GeneRatio, BgRatio, pvalue, p.adjust, qvalue, geneID, Count)

# Create the enrichResult object
enrichres <- new("enrichResult",
                 result = enrichres_formatted,
                 pvalueCutoff = 0.05,
                 pAdjustMethod = "BH",
                 qvalueCutoff = 0.2,
                 organism = "unknown",
                 ontology = "KEGG",
                 gene = character(),
                 keytype = "ENTREZID",
                 universe = character(),
                 geneSets = list())

# Barplot
barplot(enrichres, showCategory = 20) 
mutate(enrichres, qscore = -log(p.adjust, base = 10)) %>% 
  barplot(x = "-log10(FDR)")