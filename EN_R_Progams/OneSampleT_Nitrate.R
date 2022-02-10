# One-sample T-test for Nitrate data


# Load necessary libraries
library(ggplot2)
library(psych)

# Confidence intervals for sigma^2
cisig2 <- function(y,alpha=0.05){
s2 <- var(y)
n <- length(y)
lower <- (n-1)*s2/qchisq(1-alpha/2,n-1)
upper <- (n-1)*s2/qchisq(alpha/2,n-1)
return(c(lower,upper))
}

getwd()
setwd("H:/My Drive/02EnvirExchNetwkGrant/ENGrantDueProducts/EN_R_DataSources")

# Read in data set
Nitrate<- read_csv("OneSampleT_Nitrates.csv",
                      col_names = TRUE, 
                      cols(
                      mgl = col_integer()
                      ))


# Print the data
cat("One-sample T-test for Nitrate data:\n")
cat("Nitrate:\n")
print(Nitrate)

# Graphics using ggplot2
G1 <- ggplot(Nitrate,aes(x=mgl))+
      geom_histogram(binwidth=1)

G1

# Normal quantile plot
attach(Nitrate)
qqnorm(mgl)
detach(Nitrate)

# Descriptive statistics
cat("\nDescriptive statistics:\n")
attach(Nitrate)
print(describe(mgl))
detach(Nitrate)

# One-sample t test and confidence intervals
attach(Nitrate)
cat("\nOne-sample t test:\n")
print(t.test(mgl,mu=30))
detach(Nitrate)

# Confidence intervals for sigma^2 and sigma
attach(Nitrate)
cisig2out <- cisig2(mgl)
cat("95 percent confidence interval for sigma^2:\n")
print(cisig2out)
cat("95 percent confidence interval for sigma:\n")
print(sqrt(cisig2out))
detach(Nitrate)


