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

# Interactive file selection
enriches <- 


# Barplot
barplot(enrichres, showCategory = 20) 
mutate(enrichres, qscore = -log(p.adjust, base = 10)) %>% 
  barplot(x = "-log10(FDR)")