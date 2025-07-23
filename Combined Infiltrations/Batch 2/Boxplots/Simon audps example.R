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
#Data Processing
Batch_1$Treatment[Batch_1$Treatment == "EHA105-I"] <- "EHA105.I"
Batch_1$Treatment[Batch_1$Treatment == "Water-I"] <- "Water.I"
Batch_1$Treatment[Batch_1$Treatment == "GFP-I"] <- "GFP.I"
Batch_1$Treatment[Batch_1$Treatment == "GFP-M"] <- "GFP.M"

Batch_2 <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 2/Batch 2 Scores.csv", 
                    header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Batch_2 <- na.omit(Batch_2)
#Data Processing
Batch_2$Treatment[Batch_2$Treatment == "Ctrl-HP-I"] <- "Ctrl.HP.I"
Batch_2$Treatment[Batch_2$Treatment == "EHA105-I"] <- "EHA105.I"
Batch_2$Treatment[Batch_2$Treatment == "Water-I"] <- "Water.I"
Batch_2$Treatment[Batch_2$Treatment == "GFP-I"] <- "GFP.I"
Batch_2$Treatment[Batch_2$Treatment == "GFP-M"] <- "GFP.M"

Batch_7 <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 7/Batch 7 Disease.csv", 
                    header = TRUE, sep = ",", quote = "\"",
                    dec = ".", fill = TRUE, comment.char = "")
Batch_7 <- na.omit(Batch_7)
Batch_7 <- subset(Batch_7, Treatment != "Ctrl.HP.I")
Batch_7$Treatment[Batch_7$Treatment == "EHA105-I"] <- "EHA105.I"
Batch_7$Treatment[Batch_7$Treatment == "Water-I"] <- "Water.I"
Batch_7$Treatment[Batch_7$Treatment == "GFP-I"] <- "GFP.I"
Batch_7$Treatment[Batch_7$Treatment == "GFP-M"] <- "GFP.M"

#Batch 1
# Leaf Damage
B1_leaf_damage_cld
B1_leaf_damage_summary <- Batch_1 %>%
  group_by(Treatment) %>%
  summarise(Max = max(X5.dpi.Leaf.Damage)) %>%
  left_join(B1_leaf_damage_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_1, aes(x = Treatment, y = X5.dpi.Leaf.Damage, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B1_leaf_damage_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  theme_classic() +
  labs(title = "Col-0 Batch 1", subtitle = "Leaf Damage Scores 5 d.p.i",
       x = "Treatment", y = "Disease Index Scores 0-10")
box_audps
# Chlorosis 
B1_chlorosis_cld
B1_chlorosis_summary <- Batch_1 %>%
  group_by(Treatment) %>%
  summarise(Max = max(Chlorosis)) %>%
  left_join(B1_chlorosis_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_1, aes(x = Treatment, y = Chlorosis, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B1_chlorosis_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  theme_classic() +
  labs(title = "Col-0 Batch 1", subtitle = "Chlorosis Scores 5 d.p.i",
       x = "Treatment", y = "Disease Index Scores 0-10")
box_audps

#Batch 2
# Leaf Damage
B2_leaf_damage_cld
B2_leaf_damage_summary <- Batch_2 %>%
  group_by(Treatment) %>%
  summarise(Max = max(X5.dpi.Leaf.Damage)) %>%
  left_join(B2_leaf_damage_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_2, aes(x = Treatment, y = X5.dpi.Leaf.Damage, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B2_leaf_damage_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  theme_classic() +
  labs(title = "Col-0 Batch 2", subtitle = "Leaf Damage Scores 5 d.p.i",
       x = "Treatment", y = "Disease Index Scores 0-10")
box_audps
# Chlorosis 
B2_chlorosis_cld
B2_chlorosis_summary <- Batch_2 %>%
  group_by(Treatment) %>%
  summarise(Max = max(X5.dpi.Chlorosis)) %>%
  left_join(B2_chlorosis_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_2, aes(x = Treatment, y = X5.dpi.Chlorosis, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B2_chlorosis_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  theme_classic() +
  labs(title = "Col-0 Batch 2", subtitle = "Chlorosis Scores 5 d.p.i",
       x = "Treatment", y = "Disease Index Scores 0-10")
box_audps

#Batch 7
# Leaf Damage
B7_leaf_damage_cld
B7_leaf_damage_summary <- Batch_7 %>%
  group_by(Treatment) %>%
  summarise(Max = max(PS..5.dpi.Leaf.Damage)) %>%
  left_join(B7_leaf_damage_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_7, aes(x = Treatment, y = PS..5.dpi.Leaf.Damage, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B7_leaf_damage_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  theme_classic() +
  labs(title = "Col-0 Batch 7", subtitle = "Leaf Damage Scores 5 d.p.i",
       x = "Treatment", y = "Disease Index Scores 0-10")
box_audps

# Chlorosis 
B7_chlorosis_cld
B7_chlorosis_summary <- Batch_7 %>%
  group_by(Treatment) %>%
  summarise(Max = max(PS..5.dpi.Chlorosis)) %>%
  left_join(B7_chlorosis_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_7, aes(x = Treatment, y = PS..5.dpi.Chlorosis, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B7_chlorosis_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  theme_classic() +
  labs(title = "Col-0 Batch 7", subtitle = "Chlorosis Scores 5 d.p.i",
       x = "Treatment", y = "Disease Index Scores 0-10")
box_audps
box_audps