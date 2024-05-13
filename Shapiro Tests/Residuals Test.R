# Residuals Test
x<-resid(lm(Result_AUDPC ~ Result_Type * Result_Genotype, data=anv.data))
hist(x, main = "Residual Distribution of Regression Model for AUDPC by Type and Genotype")
shapiro.test(x)
m <- mean(x)
s <- sd(x)
ks.test(x,"pnorm",m,s)