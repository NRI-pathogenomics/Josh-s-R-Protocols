#DEGs post-process
library(dplyr)

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
