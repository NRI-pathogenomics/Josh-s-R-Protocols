## Batches Normality Test

Path_Assay <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 1/Batch 1 Scores.csv", 
                       header = TRUE, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE, comment.char = "")
Path_Assay <- na.omit(Path_Assay)
Batch_1_LD <- shapiro.test(Path_Assay$Leaf.Damage)
Path_Assay <- read.csv(file="/Users/joshhoti/Library/CloudStorage/OneDrive-UniversityofKent/Postgraduate/Josh R Protocols/Combined Infiltrations/Batch 2/Batch 2 Scores.csv", 
                       header = TRUE, sep = ",", quote = "\"",
                       dec = ".", fill = TRUE, comment.char = "")
Path_Assay <- na.omit(Path_Assay)
Batch_2_LD <- shapiro.test(Path_Assay$Leaf.Damage)
Batch_2_CL <- shapiro.test(Path_Assay$Chlorosis)

