# audps boxplot
box_audps <- ggplot(audps_43, aes(x=treat, y=AUDPS, fill=inoculum)) +
  geom_boxplot() +
  theme_classic() +
  labs(title="Fuggle", subtitle = "Pilgrim",  x ="Treatment", legend = "Treatment") +
  theme(plot.title = element_text(size = 13, hjust=0.25, vjust = -7), plot.subtitle = element_text(size = 13, hjust=0.75)) +
  scale_x_discrete(labels = rep_len(c("11014","11016","11027","11034","11044","control"),length.out = 12))
box_audps