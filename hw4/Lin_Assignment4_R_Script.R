setwd("C:\\Users\\Lin\\Desktop\\2019 Fall\\555 DAV_ R\\Homework\\hw4")

# 1.read the csv file
ocpt = read.csv("occupation.csv")
ocpt

summary(ocpt)

# 2.draw scatterplot and examine the association

plot(ocpt$edu, ocpt$prestige_score,
     main="Scatterplot of Prestige Score versus Education Level",
     ylab = "Prestige Score", xlab="Education Level (Years)", 
     pch = 8, col="seagreen3",
     cex=1, cex.lab = 1.5, cex.main = 1.5)
abline(m_edu,lty=3,col="black")
# calculate the correlation
cor(ocpt$edu, ocpt$prestige_score)
cor(ocpt$income, ocpt$prestige_score)
cor(ocpt$women_pctl, ocpt$prestige_score)

# 3. perform a simple linear regression
m_edu <- lm(ocpt$prestige_score~ocpt$edu)
summary(m_edu)
plot(ocpt$edu,resid(m_edu), axes=TRUE, frame.plot=TRUE, xlab = "Education Level(Years)", ylab="residuals")
abline(h=0)

hist(resid(m_edu))

# test R^2 the removal of influence point
ocpt_copy = ocpt
ocpt <- ocpt[-c(53),]
ocpt <- ocpt[-c(67),]

# 4. multiple linear regression
m_all <- lm(ocpt$prestige_score~ocpt$edu+ocpt$income+ocpt$women_pctl)
summary(m_all)
confint(m_all, level = .95)


# 6. residual plot
plot(fitted(m_all),resid(m_all), axes=TRUE, frame.plot=TRUE, xlab = "fitted values", ylab="residuals")
abline(h=0)
plot(m_all)