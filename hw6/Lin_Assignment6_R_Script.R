setwd("C:\\Users\\Lin\\Desktop\\2019 Fall\\555 DAV_ R\\Homework\\hw6")

# read the human.csv
data = read.csv("human.csv")
data

data$temp_level = ifelse(data$temp >= 98.6, 1,0)

# get numbers of each group
# males = 1, females = 2
length(which(data$sex==1 & data$temp_level == 0)) # man lower
length(which(data$sex==1 & data$temp_level == 1)) # man higher
length(which(data$sex==2 & data$temp_level == 0)) # women lower
length(which(data$sex==2 & data$temp_level == 1)) # women higher
# or using table()
table(data$sex, data$temp_level)

# higher in women and men
p1 <- length(which(data$sex==2 & data$temp_level == 1)) / 65
p2 <- length(which(data$sex==1 & data$temp_level == 1)) / 65
p1 - p2  # risk difference

# two-sample tests for proportion
prop.test(c(35,14), c(65,65), alternative = "two.sided", 
          conf.level = 0.95, correct = FALSE)

# simple logistic regression model
data$sex2 <- ifelse(data$sex == 1,"M","F")
data$sex2 <- factor(data$sex2, levels = c("M","F"))
data$female <- ifelse(data$sex2 == "F", 2, 1)
m <- glm(data$temp_level ~ data$female, family = binomial)
summary(m)
#m1 <- glm(data$temp_level ~ data$sex,family = binomial)
#summary(m1)

# conf int
library(aod)
wald.test(b = coef(m), Sigma = vcov(m), Term = 2:2)
exp(cbind(OR = coef(m), confint.default(m)))

# plotting ROC Curve
library(pROC)
data$prob1 <- predict(m, type = c("response"))
g <- roc(data$temp_level ~ data$prob1)
plot(1-g$specificities, g$sensitivities, type = "l", 
     xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curve Associated with Gender")
abline(a=0,b=1)
grid()

# Multiple logistic regression (sex, heart rate)
m2 <- glm(data$temp_level ~ data$heart_rate+data$female)
summary(m2)
wald.test(b = coef(m2), Sigma = vcov(m2), Term = 2:3)
exp(cbind(OR = coef(m2), confint.default(m2)))
# for one unit
exp(m2$coefficients[2])
# for 10 unit
exp(m2$coefficients[2]*10)

# model2 curve
data$prob2 <- predict(m2, type = c("response"))
g2 <- roc(data$temp_level ~ data$prob2)
plot(1-g2$specificities, g2$sensitivities, type = "l", 
     xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curve Associated with Heart Rate and Gender")
abline(a=0,b=1)
grid()
