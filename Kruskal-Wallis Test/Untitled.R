#Kruskal-Wallis Test
data <- Results %>% select(Result_Genotype,Result_Type, Result_AUDPC)

rownames(data) <- NULL

#convert rows to required format

data$Result_AUDPC <- as.numeric(data$Result_AUDPC)
data$Result_Genotype <- as.factor(unlist(data$Result_Genotype))
data$Result_Type <- as.factor(unlist(data$Result_Type))

### Step 3: Perform Kruskal-Wallis Test

KW_results <- kruskal.test(Result_AUDPC, formula=Result_AUDPC ~ Result_Genotype * Result_Type, data = data )
print(KW_results)

