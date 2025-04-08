####LMER of first path test####
# first, convert "spidermite" comment into "insect_damage" for consistency
test1a$comment <- gsub("spidermite", "insect_damage", test1a$comment)
# now convert insect damage comments into 1 and absence of insect damage to 0
test1a$insect_dam <- ifelse(test1a$comment == "insect_damage", 1, 0)
#eyeball the new insect damage data
ggplot(test1a, aes(y = insect_dam, x = DSI)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.5) +
  theme_classic(base_size = 15)
#Fit the linear mixed effects regression (lmer) models:
#dsi ~ treatment data, with insect damage and block as random effects on the intercept
#Complete model:
lmer1a <- lmer(DSI ~ mock_inoc + variety + mock_inoc:variety + (1 | block2) + (1 | insect_dam), data = test1a)
summary(lmer1a)
#remove highest level term i.e. the interaction
lmer1b <- lmer(DSI ~ mock_inoc + variety + (1 | block2) + (1 | insect_dam), data = test1a)
#check if removing interaction sig. affects model
anova(lmer1a,lmer1b) # chi-sq  significant, don't remove interaction term
#remove next level terms, the random effects for intercepts
#no insect damage RE
lmer1c <- lmer(DSI ~ mock_inoc * variety + (1 | block2), data = test1a)
#no block RE
lmer1d <- lmer(DSI ~ mock_inoc * variety+ (1 | insect_dam), data = test1a)
#check if either RE can be removed without affecting model
anova(lmer1a, lmer1c) # chi-sq NOT sig., remove insect_damage RE
anova(lmer1a, lmer1d) # chi-sq NOT sig., remove block RE
# no effect of block or insect damage as RE
# no random effects need to be included in the model
# ANOVA preferred model
summary(lmer1e)
# minimally sufficient model (lowest AIC) = lmer2d
