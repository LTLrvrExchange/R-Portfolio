#These statistical methods are were compiled and developed by Redwood Valley Rancheria, 2021. We make no claims at to the accuracy
#or functionality of code.

#Users are welcome to use and modify code.

#One-Sample, Paired,  and Unpaired T-tests
#
# Welch's T-Test
# Welch's t-test, aka "unpaired" or "independent sample" t-test is a two-sample location test which is used to test 
# the hypothesis that two populations have equal means. It is ore reliable than the students t-test when the two samples 
# have unequal variances and/or unequal sample sizes.

#NOTE: Example Dataset was generated and approximated using:
#Kneitel, J. M., & Lessin, C. L. (2010). Ecosystem-phase interactions: aquatic 
#eutrophication decreases terrestrial plant diversity in California vernal pools.
#Oecologia, 163(2), 461-469.

# Load necessary libraries


library(multcomp)
library(DescTools)
library(tidyverse) #tidyverse must go after multcomp to avoid conflicts
library(ggplot2)
library(tibble)
library(MASS)


# Read in data set
getwd()
#setwd("C)

prep <- read.csv (file ="PairedT-test.csv",
                header = TRUE, 
                sep = ",")


glimpse(prep)

prep %>%  #First group the data by Month
  dplyr::summarise(N = n(), #specifies that dplyr package should be used, otherwise computer gets confused.
                   #sum = sum(F_atmp),
                   #N    = length(fert),
                   mean = mean(fert),
                   median = median(fert),
                   sd   = sd(fert),
                   se   = sd / sqrt(N) )

#Visualize the data using a boxplot

boxplot(prep$fert)

boxplot(prep$fert, prep$control)

result <- t.test(prep$fert, mu=17)

result


#Check for normality

a <- qnorm(prep$fert)
a
b <- qline(fert)
b
n

#The One Sample t Test examines whether the mean of a population is statistically different from a known or hypothesized value.
#The One Sample t Test is a parametric test.
t_stat <- mean(fert)

t.test(prep$fert,prep$control)

t.test(prep$fert, prep$control
       paired = FALSE,
       conf.l)


t.test()


# One-way ANOVA

model <- aov(y ~ treat, data = kndata)
cat("\nANOVA output:\n")
summary(model)

##Pairwise Mean Comparison Methods

# Tukey HSD

tukeyout <- TukeyHSD(model, conf.level=.95)
cat("\nTukey method:\n")
tukeyout

print(confint(tukeyout))
print(cld(tukeyout))

plot(TukeyHSD(model, conf.level=.95), las = 2)


# Dunnett Test
kndata$treat <- as.factor(kndata$treat)# necessary to coerce data to "finite"
dunnettout <- DunnettTest(x=kndata$y, g=kndata$treat)
cat("\nDunnett Method:\n")
dunnettout



# Diagnostic plots to check ANOVA assumptions
p <- predict(model)
r <- resid(model)
plot(p,r)
qqnorm(r)
