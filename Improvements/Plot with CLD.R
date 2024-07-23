#plot the data with cld
#fill is based on Treatment so there's only need for 2 colours on the bar plot
#geomerrobar uses the SE column to add the error bars
Average_AUDPC
CLD
ggplot(data = Average_AUDPC, aes(x = Genotype, y = Average_AUDPC, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Average_AUDPC - SE, ymax = Average_AUDPC + SE),
                position = position_dodge(0.7), width = 0.25) +
  labs(title = "Average AUDPC with Standard Error by Genotype and Treatment",
       x = "Genotype",
       y = "Average AUDPC") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))