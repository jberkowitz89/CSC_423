#Project 4
#Read in Heart Attack Dataset
df <- read.table("heartAttack.txt", header = T)
head(df)
attach(df)

#Logistic Regression Equation
glm1 <- glm(ha2 ~ ang + sco, family = "binomial")
summary(glm1)

#Exponentiate the logit coefficients
expon <- exp(coef(glm1))
print(expon)

#Odds ratios and confidence intervals
confidenceOdds <- exp(cbind(OR = coef(glm1), confint(glm1)))
print(confidenceOdds)

#New Data with just independent variables
df2 <- data.frame(ang, sco)
head(df2)

#Predicted Probabilities
predictedProbs <- predict(glm1, df2, type="response")
print(predictedProbs)
