# Residuals Test
x<-resid(lm(Result_AUDPC ~ Result_Type * Result_Genotype, data=anv.data))
hist(x, main = "Residual Distribution of Regression Model for AUDPC by Type and Genotype")
shapiro.test(x)
m <- mean(x)
s <- sd(x)
ks.test(x,"pnorm",m,s)

# Transform Data if the residuals are not normally distributed

AUDPC_Values <- as.numeric(Results$Result_AUDPC)
log_AUDPC_Values <- log(AUDPC_Values)

# Add Log values to anv.data

anv.data$Result_AUDPC_Log10 <- log_AUDPC_Values

#Residuals Test on Log values

x<-resid(lm(Result_AUDPC_Log10 ~ Result_Type * Result_Genotype, data=anv.data))
hist(x, main = "Residual Distribution of Regression Model for log10(AUDPC) by Type and Genotype")
shapiro.test(x)
m <- mean(x)
s <- sd(x)
ks.test(x,"pnorm",m,s)