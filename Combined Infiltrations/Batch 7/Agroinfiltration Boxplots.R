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

Batch_7 <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 7/Batch 7 Agro.csv", 
                    header = TRUE, sep = ",", quote = "\"",
                    dec = ".", fill = TRUE, comment.char = "")

Batch_7$Treatment[Batch_7$Treatment == "Ctrl-HP"] <- "Ctrl.HP"
# data processing
Batch_7 <- subset(Batch_7, select = -c(PS..0.dpi.Leaf.Damage,PS..0.dpi.Chlorosis, PS..5.dpi.Leaf.Damage, PS..5.dpi.Chlorosis, Infiltrated.with.P.syringae, Fungus.gnats, Perforation))
Batch_7 <- na.omit(Batch_7) #removes NA val, ues


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

#Batch 7
# Leaf Damage
B7_leaf_damage_cld
B7_leaf_damage_summary <- Batch_7 %>%
  group_by(Treatment) %>%
  summarise(Max = max(Agro..5.dpi.Leaf.Damage)) %>%
  left_join(B7_leaf_damage_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_7, aes(x = Treatment, y = Agro..5.dpi.Leaf.Damage, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B7_leaf_damage_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  theme_classic() +
  geom_text(data = Rep_Count_B7,
            aes(x = Treatment, y = -0.5, label = paste0("n=", Rep_Number)),  # Position below x-axis
            size = 4, color = "black") +
  labs(title = "Col-0 Batch 7 Agroinfiltration Leaf Damage Scores 5 d.p.i", subtitle = B7_leaf_damage_formula,
       x = "Treatment", y = "Disease Index Scores 0-10")
print(box_audps)

# Chlorosis 
B7_chlorosis_cld
B7_chlorosis_summary <- Batch_7 %>%
  group_by(Treatment) %>%
  summarise(Max = max(Agro..5.dpi.Chlorosis)) %>%
  left_join(B7_chlorosis_cld, by = c("Treatment" = "Group"))

box_audps <- ggplot(Batch_7, aes(x = Treatment, y = Agro..5.dpi.Chlorosis, fill = Treatment)) +
  geom_boxplot() +
  geom_text(data = B7_chlorosis_summary,
            aes(x = Treatment, y = Max + 0.5, label = Letter),  # adjust y offset as needed
            size = 5, fontface = "bold") +
  theme_classic() +
  geom_text(data = Rep_Count_B7,
            aes(x = Treatment, y = -0.5, label = paste0("n=", Rep_Number)),  # Position below x-axis
            size = 4, color = "black") +
  labs(title = "Col-0 Batch 7 Agroinfiltration Chlorosis Scores 5 d.p.i", subtitle = B7_chlorosis_formula,
       x = "Treatment", y = "Disease Index Scores 0-10")
print(box_audps)
box_audps