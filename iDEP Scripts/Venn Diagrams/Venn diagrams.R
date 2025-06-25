#subset data for RNA seq analysis in idep
setwd("C:/Users/hc474/OneDrive - University of Kent/Papers/Sam RNA paper/")
# Load library
library(VennDiagram)
df1<-read.csv("deg_sig_genes_DESeq2 CULT.csv") # change these to the names you have called your files
df2<-read.csv("deg_sig_genes_DESeq2 ONTO.csv")
df3<-read.csv("deg_sig_genes_DESeq2 TISSUE.csv")
# Generate 3 sets of 200 words
set1 <- subset(df1, I.Cultivar_HAPIL.Treatment_INFECTED %in% c('Up','Down'))
set2 <- subset(df2, I.Tissue_OLD_LEAF.Treatment_INFECTED %in% c('Up','Down'))
set3 <- subset(df3, I.Tissue_FRUIT.Treatment_INFECTED %in% c('Up','Down'))
set1<- set1$gene_id
set2<- set2$gene_id
set3<- set3$gene_id
# Prepare a palette of 3 colors with R colorbrewer:
library(RColorBrewer)
myCol <- brewer.pal(3, "Pastel2")

# Chart
venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Cultivar" , "Ontogenic " , "Tissue"),
  filename = 'all venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)

# make down regulated plot

set1 <- subset(df1, I.Cultivar_HAPIL.Treatment_INFECTED %in% c('Down'))
set2 <- subset(df2, I.Tissue_OLD_LEAF.Treatment_INFECTED %in% c('Down'))
set3 <- subset(df3, I.Tissue_FRUIT.Treatment_INFECTED %in% c('Down'))
set1<- set1$gene_id
set2<- set2$gene_id
set3<- set3$gene_id

venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Cultivar" , "Ontogenic " , "Tissue"),
  filename = 'down reg venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)

# make up regulated plot

set1 <- subset(df1, I.Cultivar_HAPIL.Treatment_INFECTED %in% c('Up'))
set2 <- subset(df2, I.Tissue_OLD_LEAF.Treatment_INFECTED %in% c('Up'))
set3 <- subset(df3, I.Tissue_FRUIT.Treatment_INFECTED %in% c('Up'))
set1<- set1$gene_id
set2<- set2$gene_id
set3<- set3$gene_id

venn.diagram(
  x = list(set1, set2, set3),
  category.names = c("Cultivar" , "Ontogenic " , "Tissue"),
  filename = 'Up reg venn_diagramm.png',
  output=TRUE,
  
  # Output features
  imagetype="png" ,
  height = 480 , 
  width = 480 , 
  resolution = 300,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1
)

