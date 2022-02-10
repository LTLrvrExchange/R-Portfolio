#These analytical methods are were compiled by Redwood Valley Rancheria, 2021. We make no claims at to the accuracy for functionalty of code.
#Users are welcome to use and modify code.
#
# Oneway-ANOVA

# Load necessary libraries


library(multcomp)
library(DescTools)
library(tidyverse) #tidyverse must go after multcomp to avoid conflicts
library(ggplot2)
library (tibble)


# Read in data set
getwd()
#setwd("C)


kndata <- read_csv("Kneitel_2010_Algea.csv",
                   col_names = TRUE, 
                   cols(
                     treat = col_character(),
                     richness = col_integer(),
                     total = col_integer(),
                     algae = col_integer()
                   ))

#view data
#kndata <- as_tibble(kndata)
glimpse(kndata)



#NOTE: Example Dataset was generated and approximated using:
#Kneitel, J. M., & Lessin, C. L. (2010). Ecosystem-phase interactions: aquatic 
#eutrophication decreases terrestrial plant diversity in California vernal pools.
#Oecologia, 163(2), 461-469.



# Apply transformations and add columns to kndata

kndata <- transform(kndata, vcover=total-algae)
kndata <- transform(kndata, y = asin(sqrt(vcover/100)))
cat("Multiple comparisons for algae cover\n")
cat("Data from Kneitel and Lessin (2010)\n")
cat("kndata:\n")
print(kndata)

# Graphics using ggplot2
G1 <- ggplot(kndata, aes(x = factor(treat), y = y))+
  geom_jitter(size=2, shape=8, position=position_jitter(width=0.1))+
  stat_summary(fun="mean",geom="point",colour = "red", size=3)+
  stat_summary(fun.data="mean_cl_normal",geom="errorbar",width=0.2,size=1)

G1

# levels(kndata$treat) <- abbreviate(levels(kndata$treat), 6)
G2 <- ggplot(kndata, aes(x = factor(treat), y = y))+
  geom_boxplot()+
  ggtitle("Algea Cover by Treatment")+
  labs(x="Treatment", y="Algea Cover = asin(sqrt(vcover/100)")

G2

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
