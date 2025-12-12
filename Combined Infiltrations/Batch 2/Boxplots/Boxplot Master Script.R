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

#count number of replicates
Treat_Count_B1 <- list()
Treat_Count_B1$Treatment <- as.list(unique(Batch_1$Treatment))
counter <- length(Treat_Count_B1$Treatment)
Rep_Count_B1 <-  data.frame(matrix(ncol = 2, nrow = counter))
colnames(Rep_Count_B1) <- c("Treatment", "Rep_Number")
Rep_Count_B1$Treatment <- unique(Batch_1$Treatment)
for(i in 1:counter){
  print(Treat_Count_B1$Treatment[i])
  count <- Batch_1 %>% filter(Treatment == Treat_Count_B1$Treatment[i]) %>% nrow()
  Rep_Count_B1$Rep_Number[i] <- count
}
Rep_Count_B1

Batch_2 <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 2/Batch 2 Scores.csv", 
                    header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
Batch_2 <- na.omit(Batch_2)
#Data Processing
Batch_2$Treatment[Batch_2$Treatment == "Ctrl-HP-I"] <- "Ctrl.HP.I"
Batch_2$Treatment[Batch_2$Treatment == "EHA105-I"] <- "EHA105.I"
Batch_2$Treatment[Batch_2$Treatment == "Water-I"] <- "Water.I"
Batch_2$Treatment[Batch_2$Treatment == "GFP-I"] <- "GFP.I"
Batch_2$Treatment[Batch_2$Treatment == "GFP-M"] <- "GFP.M"

#count number of replicates
Treat_Count_B2 <- list()
Treat_Count_B2$Treatment <- as.list(unique(Batch_2$Treatment))
counter <- length(Treat_Count_B2$Treatment)
Rep_Count_B2 <-  data.frame(matrix(ncol = 2, nrow = counter))
colnames(Rep_Count_B2) <- c("Treatment", "Rep_Number")
Rep_Count_B2$Treatment <- unique(Batch_2$Treatment)
for(i in 1:counter){
  print(Treat_Count_B2$Treatment[i])
  count <- Batch_2 %>% filter(Treatment == Treat_Count_B2$Treatment[i]) %>% nrow()
  Rep_Count_B2$Rep_Number[i] <- count
}
Rep_Count_B2

Batch_7 <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 7/Batch 7 diff.csv", 
                    header = TRUE, sep = ",", quote = "\"",
                    dec = ".", fill = TRUE, comment.char = "")
Batch_7 <- na.omit(Batch_7)
Batch_7 <- subset(Batch_7, Treatment != "Ctrl.HP.I")
Batch_7$Treatment[Batch_7$Treatment == "EHA105-I"] <- "EHA105.I"
Batch_7$Treatment[Batch_7$Treatment == "Water-I"] <- "Water.I"
Batch_7$Treatment[Batch_7$Treatment == "GFP-I"] <- "GFP.I"
Batch_7$Treatment[Batch_7$Treatment == "GFP-M"] <- "GFP.M"

#count number of replicates
Treat_Count_B7 <- list()
Treat_Count_B7$Treatment <- as.list(unique(Batch_7$Treatment))
counter <- length(Treat_Count_B7$Treatment)
Rep_Count_B7 <-  data.frame(matrix(ncol = 2, nrow = counter))
colnames(Rep_Count_B7) <- c("Treatment", "Rep_Number")
Rep_Count_B7$Treatment <- unique(Batch_7$Treatment)
for(i in 1:counter){
  print(Treat_Count_B7$Treatment[i])
  count <- Batch_7 %>% filter(Treatment == Treat_Count_B7$Treatment[i]) %>% nrow()
  Rep_Count_B7$Rep_Number[i] <- count
}
Rep_Count_B7

#Batch 1
# Leaf Damage
B1_leaf_damage_cld
B1_leaf_damage_summary <- Batch_1 %>%
  group_by(Treatment) %>%
  summarise(
    Average = mean(X5.dpi.Leaf.Damage, na.rm = TRUE),
    SE = sd(X5.dpi.Leaf.Damage, na.rm = TRUE) / sqrt(n()),
    Max = max(X5.dpi.Leaf.Damage, na.rm = TRUE),
    Min = min(X5.dpi.Leaf.Damage, na.rm = TRUE)
  ) %>%
  left_join(B1_leaf_damage_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_1, aes(x = Treatment, y = X5.dpi.Leaf.Damage, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B1_leaf_damage_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  geom_text(data = Rep_Count_B1,
            aes(x = Treatment, y = -0.5, label = paste0("n=", Rep_Number)),  # Position below x-axis
            size = 4, color = "black") +
  theme_classic() +
  labs(title = "Col-0 Batch 1 Leaf Damage Scores 5 d.p.i", subtitle = B1_leaf_damage_formula,
       x = "Treatment", y = "Disease Index Scores 0-10")
print(box_audps)
# Chlorosis 
B1_chlorosis_cld
B1_chlorosis_summary <- Batch_1 %>%
  group_by(Treatment) %>%
  summarise(
    Average = mean(Chlorosis, na.rm = TRUE),
    SE = sd(Chlorosis, na.rm = TRUE) / sqrt(n()),
    Max = max(Chlorosis, na.rm = TRUE),
    Min = min(Chlorosis, na.rm = TRUE)
  ) %>%
  left_join(B1_chlorosis_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_1, aes(x = Treatment, y = Chlorosis, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B1_chlorosis_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  geom_text(data = Rep_Count_B1,
            aes(x = Treatment, y = -0.5, label = paste0("n=", Rep_Number)),  # Position below x-axis
            size = 4, color = "black") +
  theme_classic() +
  labs(title = "Col-0 Batch 1 Chlorosis Scores 5 d.p.i", subtitle = B1_chlorosis_formula,
       x = "Treatment", y = "Disease Index Scores 0-10")
print(box_audps)

#Batch 2
# Leaf Damage
B2_leaf_damage_cld
B2_chlorosis_summary <- Batch_2 %>%
  group_by(Treatment) %>%
  summarise(
    Average = mean(X5.dpi.Leaf.Damage, na.rm = TRUE),
    SE = sd(X5.dpi.Leaf.Damage, na.rm = TRUE) / sqrt(n()),
    Max = max(X5.dpi.Leaf.Damage, na.rm = TRUE),
    Min = min(X5.dpi.Leaf.Damage, na.rm = TRUE)
  ) %>%
  left_join(B2_chlorosis_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_2, aes(x = Treatment, y = X5.dpi.Leaf.Damage, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B2_leaf_damage_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  geom_text(data = Rep_Count_B2,
            aes(x = Treatment, y = -0.5, label = paste0("n=", Rep_Number)),  # Position below x-axis
            size = 4, color = "black") +
  theme_classic() +
  labs(title = "Col-0 Batch 2 Leaf Damage Scores 5 d.p.i", subtitle = B2_leaf_damage_formula,
       x = "Treatment", y = "Disease Index Scores 0-10")
print(box_audps)
# Chlorosis 
B2_chlorosis_cld
B2_chlorosis_summary <- Batch_2 %>%
  group_by(Treatment) %>%
  summarise(
    Average = mean(X5.dpi.Chlorosis, na.rm = TRUE),
    SE = sd(X5.dpi.Chlorosis, na.rm = TRUE) / sqrt(n()),
    Max = max(X5.dpi.Chlorosis, na.rm = TRUE),
    Min = min(X5.dpi.Chlorosis, na.rm = TRUE)
  ) %>%
  left_join(B2_chlorosis_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_2, aes(x = Treatment, y = X5.dpi.Chlorosis, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B2_chlorosis_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  theme_classic() +
  geom_text(data = Rep_Count_B2,
            aes(x = Treatment, y = -0.5, label = paste0("n=", Rep_Number)),  # Position below x-axis
            size = 4, color = "black") +
  labs(title = "Col-0 Batch 2 Chlorosis Scores 5 d.p.i", subtitle = B2_chlorosis_formula,
       x = "Treatment", y = "Disease Index Scores 0-10")
print(box_audps)

#Batch 7
# Leaf Damage
B7_leaf_damage_cld
B7_leaf_damage_summary <- Batch_7 %>%
  group_by(Treatment) %>%
  summarise(
    Average = mean(Leaf.Damage.Difference, na.rm = TRUE),
    SE = sd(Leaf.Damage.Difference, na.rm = TRUE) / sqrt(n()),
    Max = max(Leaf.Damage.Difference, na.rm = TRUE),
    Min = min(Leaf.Damage.Difference, na.rm = TRUE)
  ) %>%
  left_join(B7_leaf_damage_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_7, aes(x = Treatment, y = Leaf.Damage.Difference, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B7_leaf_damage_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  geom_text(data = Rep_Count_B7,
            aes(x = Treatment, y = -0.5, label = paste0("n=", Rep_Number)),  # Position below x-axis
            size = 4, color = "black") +
  theme_classic() +
  labs(title = "Col-0 Batch 7 Leaf Damage Scores 5 d.p.i", subtitle = B7_leaf_damage_formula,
       x = "Treatment", y = "Difference between A.tumefaciens and P.syringae Disease Index Scores 0-10")
print(box_audps)

# Chlorosis 
B7_chlorosis_cld
B7_chlorosis_summary <- Batch_7 %>%
  group_by(Treatment) %>%
  summarise(
    Average = mean(Chlorosis.Difference, na.rm = TRUE),
    SE = sd(Chlorosis.Difference, na.rm = TRUE) / sqrt(n()),
    Max = max(Chlorosis.Difference, na.rm = TRUE),
    Min = min(Chlorosis.Difference, na.rm = TRUE)
  ) %>%
  left_join(B7_chlorosis_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_7, aes(x = Treatment, y = Chlorosis.Difference, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B7_chlorosis_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  geom_text(data = Rep_Count_B7,
            aes(x = Treatment, y = -0.5, label = paste0("n=", Rep_Number)),  # Position below x-axis
            size = 4, color = "black") +
  theme_classic() +
  labs(title = "Col-0 Batch 7 Chlorosis Scores 5 d.p.i", subtitle = B7_chlorosis_formula,
       x = "Treatment", y = "Difference between A.tumefaciens and P.syringae Disease Index Scores 0-10")
print(box_audps)
box_audps