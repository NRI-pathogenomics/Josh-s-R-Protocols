# audps boxplot
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tidyverse)
library(magrittr)
library(plotrix)
library(FSA)
library(dunn.test)
library(rcompanion)
Batch_1 <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 1/Batch 1 Scores.csv", 
                    header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Batch_1 <- na.omit(Batch_1)
Batch_1 <- subset(Batch_1, Treatment != "Ctrl-HP-I")
Batch_2 <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 2/Batch 2 Scores.csv", 
                    header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Batch_2 <- na.omit(Batch_2)
Batch_7 <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 7/Batch 7 Disease.csv", 
                    header = TRUE, sep = ",", quote = "\"",
                    dec = ".", fill = TRUE, comment.char = "")
Batch_7 <- na.omit(Batch_7)
Batch_7 <- subset(Batch_7, Treatment != "Ctrl.HP.I")
#Batch 1
box_audps <- ggplot(Batch_1, aes(x=Treatment, y=X0.dpi.Leaf.Damage, fill=Treatment)) +
  geom_boxplot() +
  theme_classic() +
  labs(title="Col-0 Batch 1", subtitle = "Leaf Damage Scores",  x ="Treatment", legend = "Treatment") +
  theme(plot.title = element_text(size = 13, hjust=0.25, vjust = -7), plot.subtitle = element_text(size = 13, hjust=0.75)) +
  labs(y=Batch_1$Treatment)
box_audps