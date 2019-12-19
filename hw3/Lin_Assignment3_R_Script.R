setwd("C:\\Users\\Lin\\Desktop\\2019 Fall\\555 DAV_ R\\Homework\\hw3")

# 1. read the fisherman data
mercurydata = read.csv("fisherman_mercury.csv")
mercurydata

# 2. draw scatterplot, x-axis for number of fish and y-axis for mercury content
plot(mercurydata$fish, mercurydata$mercury,
main = "Scatterplot of Mercury Content versus Number of Meals with Fish",
xlab = "Meals with Fish", ylab = "Total Mercury in Head Hair",
xlim = c(0, 25), ylim = c(0,12), pch = 4, col="red",
cex=1, cex.lab = 1.2, cex.main = 1.2)

# Calculate Sample Correlation
cor(mercurydata$fish, mercurydata$mercury)

cor.test(mercurydata$fish, mercurydata$mercury)

# calculate the fitting linear model
flm <- lm(mercurydata$mercury~mercurydata$fish)

# get Beta1 and Beta0(interception)
lm(mercurydata$mercury~mercurydata$fish)

# adding the regression line to scatterplot
plot(mercurydata$fish, mercurydata$mercury,
     main = "Scatterplot of Mercury Content versus Number of Meals with Fish",
     xlab = "Meals with Fish", ylab = "Total Mercury in Head Hair(Mg)",
     xlim = c(0, 25), ylim = c(0,12), pch = 4, col="red",
     cex=1, cex.lab = 1.2, cex.main = 1.2)
abline(flm,lty=5,col="black")

# 6. calculate the ANOVA
anova(flm)
summary(flm)

# calculate the F-test by qf()
qf(0.90, 1, 98)

# calculate conf=90% for beta1 
confint(flm, level = 0.90)
