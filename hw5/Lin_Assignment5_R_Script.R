setwd("C:/Users/Lin/Desktop/2019 Fall/555 DAV_ R/Homework/hw5")

data <- read.csv("IQtest.csv")
library(plyr)
ddply(data, "group", summarise,
              N = length(iq),
              mean = mean(iq),
              sd = sd(iq))

ddply(data, "group", summarise,
      N = length(age),
      mean = mean(age),
      sd = sd(age))

boxplot(iq~group, data = data, main = "xxx", xlab = "Group",
        ylab = "IQ")  

data$grp <- revalue(data$group, c("Chemistry student" = "Chemistry",
                    "Math student" = "Math", 
                    "Physics student" = "Physics"))

# one-way ANOVA using aov function
m <- aov(iq~grp, data = data)
summary(m)

# Pairwise comparison using Tukey's procedure
TukeyHSD(m)
plot(TukeyHSD(m), cex.axis = .7)
plot(TukeyHSD(m), cex.axis = .7, las = 2)

# create dummy variables
data$g0 <- ifelse(data$group == "Chemistry student", 1, 0)
data$g1 <- ifelse(data$group == "Math student", 1, 0)
data$g2 <- ifelse(data$group == "Physics student", 1, 0)

# One way ANOVA using lm function
mche <- lm(iq~g1+g2, data = data)
summary(mche)

# adjusting for age
library(car)
Anova(lm(iq~grp+age, data = data), type = 3)
Anova(lm(iq~grp, data = data), type = 3)

Anova(m_age, type = 3)

# Generate Least Squares means and comparisons
library(lsmeans)
options(contrasts=c("contr.treatment","contr.poly"))
m_age <- lm(iq~grp+age, data = data)
lsmeans(m_age, pairwise ~ grp, adjust = "none")
lsmeans(m_age, pairwise ~ grp, adjust = "tukey")
