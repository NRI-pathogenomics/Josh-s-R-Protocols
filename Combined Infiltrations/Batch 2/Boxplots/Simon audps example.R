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
# Leaf Damage
box_audps <- ggplot(Batch_1, aes(x=Treatment, y=X5.dpi.Leaf.Damage, fill=Treatment)) +
  geom_boxplot() +
  theme_classic() +
  labs(title="Col-0 Batch 1", subtitle = "Leaf Damage Scores 5 d.p.i",  x ="Treatment", legend = "Treatment") +
  labs(y="Disease Index Scores 0-10")
box_audps
# Chlorosis 
box_audps <- ggplot(Batch_1, aes(x=Treatment, y=Chlorosis, fill=Treatment)) +
  geom_boxplot() +
  theme_classic() +
  labs(title="Col-0 Batch 1", subtitle = "Chlorosis Scores 5 d.p.i",  x ="Treatment", legend = "Treatment") +
  labs(y="Disease Index Scores 0-10")
box_audps

#Batch 2
# Leaf Damage
box_audps <- ggplot(Batch_2, aes(x=Treatment, y=X5.dpi.Leaf.Damage, fill=Treatment)) +
  geom_boxplot() +
  theme_classic() +
  labs(title="Col-0 Batch 2", subtitle = "Leaf Damage Scores 5 d.p.i",  x ="Treatment", legend = "Treatment") +
  labs(y="Disease Index Scores 0-10")
box_audps
# Chlorosis 
box_audps <- ggplot(Batch_2, aes(x=Treatment, y=X5.dpi.Chlorosis, fill=Treatment)) +
  geom_boxplot() +
  theme_classic() +
  labs(title="Col-0 Batch 2", subtitle = "Chlorosis Scores 5 d.p.i",  x ="Treatment", legend = "Treatment") +
  labs(y="Disease Index Scores 0-10")
box_audps

#Batch 7
# Leaf Damage
box_audps <- ggplot(Batch_7, aes(x=Treatment, y=PS..5.dpi.Leaf.Damage, fill=Treatment)) +
  geom_boxplot() +
  theme_classic() +
  labs(title="Col-0 Batch 7", subtitle = "Leaf Damage Scores 5 d.p.i",  x ="Treatment", legend = "Treatment") +
  labs(y="Disease Index Scores 0-10")
box_audps
# Chlorosis 
box_audps <- ggplot(Batch_7, aes(x=Treatment, y=PS..5.dpi.Chlorosis, fill=Treatment)) +
  geom_boxplot() +
  theme_classic() +
  labs(title="Col-0 Batch 7", subtitle = "Chlorosis Scores 7 d.p.i",  x ="Treatment", legend = "Treatment") +
  labs(y="Disease Index Scores 0-10")
box_audps