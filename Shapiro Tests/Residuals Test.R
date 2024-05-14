# Residuals Test
x<-resid(lm(Result_AUDPC ~ Result_Type * Result_Genotype, data=anv.data))
hist(x, main = "Residual Distribution of Regression Model for AUDPC by Type and Genotype")
shapiro.test(x)
m <- mean(x)
s <- sd(x)
ks.test(x,"pnorm",m,s)

<<<<<<< HEAD
# Transform Data if the residuals are not normally distributed based on the boxcox(anv.model) output from the ANOVA test
=======
# Transform Data if the residuals are not normally distributed
>>>>>>> 8e0a3d371fa981273f9e121772fb64ab69d978b7

AUDPC_Values <- as.numeric(Results$Result_AUDPC)
log_AUDPC_Values <- log(AUDPC_Values)

# Add Log values to anv.data

anv.data$Result_AUDPC_Log10 <- log_AUDPC_Values

<<<<<<< HEAD
# Transform Data if the residuals are not normally distributed based on the boxcox(anv.model) output from the ANOVA test
# inverse sqroot
AUDPC_Values <- as.numeric(Results$Result_AUDPC)
Invsq_AUDPC_Values <- 1/AUDPC_Values^2

# Add Log values to anv.data

anv.data$Result_Invsq_AUDPC <- Invsq_AUDPC_Values

#Residuals Test on Log values

x<-resid(lm(Result_Invsq_AUDPC~ Result_Type * Result_Genotype, data=anv.data))
=======
#Residuals Test on Log values

x<-resid(lm(Result_AUDPC_Log10 ~ Result_Type * Result_Genotype, data=anv.data))
>>>>>>> 8e0a3d371fa981273f9e121772fb64ab69d978b7
hist(x, main = "Residual Distribution of Regression Model for log10(AUDPC) by Type and Genotype")
shapiro.test(x)
m <- mean(x)
s <- sd(x)
<<<<<<< HEAD
ks.test(x,"pnorm",m,s)


# redo shapiro.wilkes
inv.sq.anv.model <- aov(Invsq_AUDPC_Values ~ Result_Type * Result_Genotype, data = anv.data)
shapiro.test(anv.data$Result_Invsq_AUDPC)
#still don't have normally-distributed residuals
#check residuals
hist()
=======
ks.test(x,"pnorm",m,s)
>>>>>>> 8e0a3d371fa981273f9e121772fb64ab69d978b7
