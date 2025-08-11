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
  category = c("AT1703 vs Col-0 Downregulated 0 dpi", "AT1703 vs Col-0 Downregulated 2 dpi"),  # Text labels for each circle
  fill = myCol[1:2],  # Colors for the circles (first 2 colors from the Pastel2 palette)
  cat.pos = c(-20, 20),  # Position of category labels in degrees (-20° for left, +20° for right)
  cat.dist = 0.05,  # Distance of category labels from the edge of circles (0.05 = close to circles)
  cex = 2.5,
  cat.cex = 2.0
  )

# Simple upregulated genes comparison  
grid.newpage()  # Creates another new blank plotting page (clears previous plot)
draw.pairwise.venn(
  area1 = length(up_genes_0),  # Count total upregulated genes at 0 dpi (left circle size)
  area2 = length(up_genes_2),  # Count total upregulated genes at 2 dpi (right circle size)
  cross.area = length(intersect(up_genes_0, up_genes_2)),  # Count genes upregulated in BOTH conditions (overlap area)
  category = c("AT1703 vs Col-0 Upregulated 0 dpi", "AT1703 vs Col-0 Upregulated 2 dpi"),  # Text labels for each circle
  fill = myCol[1:2],  # Same colors as downregulated plot for consistency
  cat.pos = c(-20, 20),  # Same label positioning as downregulated plot
  cat.dist = 0.05,  # Same label distance as downregulated plot
  cex = 2.5,
  cat.cex = 2.0
)

#generate lists of the overlaps - genes we are not interested in
# Calculate overlaps
down_overlap <- as.list(intersect(down_genes_0, down_genes_2))
up_overlap <- as.list(intersect(up_genes_0, up_genes_2))
total_overlap <- c(up_overlap, down_overlap)

#filter the up/down genes using the intersects
sig_degs_0 <- degs_0[!degs_0$ensembl_ID %in% total_overlap, ] 
sig_degs_2 <- degs_2[!degs_2$ensembl_ID %in% total_overlap, ] 

## some genes will be downregulated on day 0 but upregulated in day 2 so if we rerun the intersect there may still be shared genes
## since any shared genes remaining after filtration should be being regulated in opposing directions (they weren't part of the up/down intersects in the venn diagrams) they should be included in the data
## just to check however, run this code and compare the genes - as long as their up/down values differ its fine

# check_overlap <- intersect(sig_degs_0$ensembl_ID, sig_degs_2$ensembl_ID)
# sig_degs_0[sig_degs_0$ensembl_ID %in% check_overlap, ]
# sig_degs_2[sig_degs_2$ensembl_ID %in% check_overlap, ]

# Identify a list of the top genes for LFC and adjusted P.value
# order the datasets for highest/lowest LFC
sig_degs_0 <- sig_degs_0 %>% arrange(desc(WT_0dpi.X1703_0dpi_log2FC))
sig_degs_2 <- sig_degs_2 %>% arrange(desc(WT_2dpi.X1703_2dpi_log2FC))

#highest/lowest LFCs
hi_LFC_0 <- head(sig_degs_0, n=25)
lo_LFC_0 <- tail(sig_degs_0, n=25)
hi_LFC_2 <- head(sig_degs_2, n=25)
lo_LFC_2 <- tail(sig_degs_2, n=25)

#order the datasets for the most significant P.value
#lower the P.value the more significant the change so asc needs to be used here, which is the default sort method for arrange
sig_degs_0 <- sig_degs_0 %>% arrange(WT_0dpi.X1703_0dpi_adjPval)
sig_degs_2 <- sig_degs_2 %>% arrange(WT_2dpi.X1703_2dpi_adjPval)

#highest/lowest adjusted P.values
hi_adj_P_value_0 <- head(sig_degs_0, n=25)
hi_adj_P_value_2 <- head(sig_degs_2, n=25)

## GO analysis
#Install and load required R packages:  
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
  }
if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
  BiocManager::install("clusterProfiler")
}
# Install the correct Arabidopsis database if you haven't already
if (!requireNamespace("org.At.tair.db", quietly = TRUE)) {
  BiocManager::install("org.At.tair.db")
}
if (!requireNamespace("GO.db", quietly = TRUE)){
  BiocManager::install("GO.db")}
library(BiocManager)
library(clusterProfiler)
library(GO.db)
library(org.At.tair.db)
# sig_degs_0
#split the sig_degs up into up and downregulated so you can analyse the GO enrichment in context of the direction genes are being regulated in
sig_degs_0_up <- subset(sig_degs_0, `Up/Down` == "Up")
sig_degs_0_down <- subset(sig_degs_0, `Up/Down` == "Down")
#Upregulated genes enrichment
sig_degs0_enrichFrame_up <- enrichGO(gene = sig_degs_0_up$ensembl_ID,
                                  OrgDb = org.At.tair.db,  # Changed from org.Hs.eg.db
                                  keyType = "TAIR",        # Changed from "ENSEMBL"
                                  ont = "ALL",
                                  pAdjustMethod = "BH",
                                  pvalueCutoff = 0.05,
                                  qvalueCutoff = 0.2)
#Downregulated genes enrichment
sig_degs0_enrichFrame_down <- enrichGO(gene = sig_degs_0_down$ensembl_ID,
                                     OrgDb = org.At.tair.db,  # Changed from org.Hs.eg.db
                                     keyType = "TAIR",        # Changed from "ENSEMBL"
                                     ont = "ALL",
                                     pAdjustMethod = "BH",
                                     pvalueCutoff = 0.05,
                                     qvalueCutoff = 0.2)
#standard P-value used here as the sig_degs have already been filtered for significance at the 0.01 level

sig_degs_2_up <- subset(sig_degs_2, `Up/Down` == "Up")
sig_degs_2_down <- subset(sig_degs_2, `Up/Down` == "Down")
#Upregulated genes enrichment
sig_degs2_enrichFrame_up <- enrichGO(gene = sig_degs_2_up$ensembl_ID,
                                     OrgDb = org.At.tair.db,  # Changed from org.Hs.eg.db
                                     keyType = "TAIR",        # Changed from "ENSEMBL"
                                     ont = "ALL",
                                     pAdjustMethod = "BH",
                                     pvalueCutoff = 0.05,
                                     qvalueCutoff = 0.2)
#Downregulated genes enrichment
sig_degs2_enrichFrame_down <- enrichGO(gene = sig_degs_2_down$ensembl_ID,
                                       OrgDb = org.At.tair.db,  # Changed from org.Hs.eg.db
                                       keyType = "TAIR",        # Changed from "ENSEMBL"
                                       ont = "ALL",
                                       pAdjustMethod = "BH",
                                       pvalueCutoff = 0.05,
                                       qvalueCutoff = 0.2)

# produce a dotplot of the top 25 genes based on P.adjust
# Install enrichplot if needed
if (!requireNamespace("enrichplot", quietly = TRUE)) {
  BiocManager::install("enrichplot")
}
# sig_degs_0
library(enrichplot)
library(ggplot2)

dotplot(sig_degs0_enrichFrame_up,
        x = "GeneRatio",
        color = "p.adjust",
        title = "Top 25 of GO Enrichment for Upregulated Genes (AT1703 vs Col-0) at 0 d.p.i",
        showCategory = 25,
        label_format = 80
) + theme(
  plot.title = element_text(size = 16, face = "bold"),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 12)
)

dotplot(sig_degs0_enrichFrame_down,
        x = "GeneRatio",
        color = "p.adjust",
        title = "Top 25 of GO Enrichment for Downregulated Genes (AT1703 vs Col-0) at 0 d.p.i",
        showCategory = 25,
        label_format = 80
) + theme(
  plot.title = element_text(size = 16, face = "bold"),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 12)
)

# sig_degs_2
dotplot(sig_degs2_enrichFrame_up,
        x = "GeneRatio",
        color = "p.adjust",
        title = "Top 25 of GO Enrichment for Upregulated Genes (AT1703 vs Col-0) at 2 d.p.i",
        showCategory = 25,
        label_format = 80
) + theme(
  plot.title = element_text(size = 16, face = "bold"),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 12)
)

dotplot(sig_degs2_enrichFrame_down,
        x = "GeneRatio",
        color = "p.adjust",
        title = "Top 25 of GO Enrichment for Downregulated Genes (AT1703 vs Col-0) at 2 d.p.i",
        showCategory = 25,
        label_format = 80
) + theme(
  plot.title = element_text(size = 16, face = "bold"),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 12)
)

# Finally export the results:
# Export the results to CSV
#sig_degs_0_up/down
write.csv(sig_degs0_enrichFrame_up@result, 
          file = "upreg_0dpi_GO_enrichment_results.csv", 
          row.names = FALSE)

write.csv(sig_degs0_enrichFrame_down@result, 
          file = "downreg_0dpi_GO_enrichment_results.csv", 
          row.names = FALSE)
#sig_degs_2_up/down
write.csv(sig_degs2_enrichFrame_up@result, 
          file = "upreg_2dpi_GO_enrichment_results.csv", 
          row.names = FALSE)

write.csv(sig_degs2_enrichFrame_down@result, 
          file = "downreg_2dpi_GO_enrichment_results.csv", 
          row.names = FALSE)