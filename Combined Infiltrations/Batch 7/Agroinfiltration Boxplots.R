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

Batch_7 <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 7/Batch 7 Disease.csv", 
                    header = TRUE, sep = ",", quote = "\"",
                    dec = ".", fill = TRUE, comment.char = "")
Batch_7 <- na.omit(Batch_7)
Batch_7 <- subset(Batch_7, Treatment != "Ctrl.HP.I")
Batch_7$Treatment[Batch_7$Treatment == "EHA105-I"] <- "EHA105.I"
Batch_7$Treatment[Batch_7$Treatment == "Water-I"] <- "Water.I"
Batch_7$Treatment[Batch_7$Treatment == "GFP-I"] <- "GFP.I"
Batch_7$Treatment[Batch_7$Treatment == "GFP-M"] <- "GFP.M"


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
  labs(title = "Col-0 Batch 7 Agroinfiltration Leaf Damage Scores 5 d.p.i", subtitle = B7_leaf_damage_formula,
       x = "Treatment", y = "Disease Index Scores 0-10")
print(box_audps)

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
  labs(title = "Col-0 Batch 7 Agroinfiltration Chlorosis Scores 5 d.p.i", subtitle = B7_chlorosis_formula,
       x = "Treatment", y = "Disease Index Scores 0-10")
print(box_audps)
box_audps