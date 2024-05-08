# Residuals Test
x<-resid(lm(Result_AUDPC ~ Result_Type * Result_Genotype, data=anv.data))
hist(x)
shapiro.test(x)
m <- mean(x)
s <- sd(x)
ks.test(x,"pnorm",m,s)