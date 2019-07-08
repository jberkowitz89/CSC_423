#Part A. ChemReaction Dataset (35 pts)
#Create a regression model for predicting the rate 
#of a chemical reaction based on the amount of the monomer 
#(molecule containing a single cluster of atoms) and the amount 
#of the dimer (molecule containing two identical clusters of atoms. 
#Here is the data:

monomer <- c(0.34, 0.34, 0.58, 1.26, 1.26, 1.82)
dimer <- c(0.73, 0.73, 0.69, 0.97, 0.97, 0.46)
rate <- c(5.75, 4.79, 5.44, 9.09, 8.59, 5.09)

chem_df <- data.frame(monomer, dimer, rate)
head(chem_df)

#Find the regression equation for predicting rate from monomer and dimer.
#Linear Regression equation
m1 <- lm(rate ~ monomer + dimer)
summary(m1)
#Checking Confidence Intervals
confint(m1, level = 0.95)

#Find predicted values of the model
pred <- predict(m1)
print(pred)
#Residuals
resid <- residuals(m1)
print(resid)

#If you try a new experiment with the amount 1.00 of monomer and 
#0.8 of dimer, what is the predicted reaction rate? Obtain the predicted 
#reaction rate by hand and also using SAS or R

new_row <- c(1.0, 0.8, NA)
chem_df2 <- rbind(chem_df, new_row)
chem_df2

#new model
m2 <- lm(rate ~ monomer + dimer, data=chem_df2)
#new data minus dependent variable
new_data <- data.frame(chem_df2$monomer, chem_df2$dimer)
colnames(new_data) <- c("monomer", "dimer")

#Find Predicted Values and prediction intervals
pred2 <- predict(m2, newdata=new_data, interval="prediction")
print(pred2)

#Find Confidence Interval and Prediction Interval
predict_interval <- predict(m2, new_data, interval="predict")
print(predict_interval)

confidence_interval <- predict(m2, new_data, interval="confidence")
print(confidence_interval)

#Use the Banking Dataset banking.txt for this part. This dataset consists 
#of data acquired from banking and census records for different zip codes 
#in the bank's current market. Such information can be useful in targeting 
#advertising for new customers or for choosing locations for branch offices.

banking <- read.table("banking.txt", header=T)
print(banking)
attach(banking)

#Create scatterplots to visualize the associations between bank 
#balance and the other five variables. Do the associations appear to be linear?
plot(Age, Balance, main="Scatter of Age v Balance", xlab="Age", ylab="Balance")
plot(Education, Balance, main="Scatter of Education v Balance", xlab="Education", ylab="Balance")
plot(Income, Balance, main="Scatter of Income v Balance", xlab="Income", ylab="Balance")
plot(HomeVal, Balance, main="Scatter of HomeVal v Balance", xlab="HomeVal", ylab="Balance")
plot(Wealth, Balance, main="Scatter of Wealth v Balance", xlab="Wealth", ylab="Balance")

#Compute correlation values of bank balance vs 
#the other variables. Interpret the correlation values. 
#Which variables appear to be strongly associated.

cor(banking)

#Fit a regression model of balance vs the other five variables. 
#Write the expression of the estimated regression model.
m3 <- lm(Balance ~ Age + Education + Income + HomeVal + Wealth)
summary(m3)

#Are there any influence points for this model?
print(influence.measures(m3))
summary(influence.measures(m3))

#A good model should only contain significant independent variables, 
#so remove the variable with the largest p-value (>0.05) and refit the 
#regression model of balance vs the remaining four predictors. Write down the 
#expression of the new regression model. Do NOT consider dropping more 
#than one insignificant variables at one time, but rather remove one 
#variable at a time. In fact, when one variable is removed from a 
#regression model, it often happens that non-significant variables in 
#the original model become significant in the reduced model.  

m4 <- lm(Balance ~ Age + Education + Income + Wealth)
summary(m4)

m5 <- lm(scale(Balance) ~ scale(Age) + scale(Education) + 
              scale(Income) + scale(Wealth))
summary(m5)
cat("standardized coeffcients:\n")
print(summary(m5))

#Anova and f-test of final model
anova(m4)

#Residual plots and normal plot of final model
r <- residuals(m4)
p <- fitted(m4)

plot(p, r, main="Residual plot of Final Banking Model", xlab="Predicted Values", ylab="Residuals")
plot(Age, r, main="Residual plot of Age vs Residuals", xlab="Age", ylab="Residuals")
plot(Education, r, main="Residual plot of Education vs Residuals", xlab="Education", ylab="Residuals")
plot(Income, r, main="Residual plot of Income vs Residuals", xlab="Income", ylab="Residuals")
plot(Wealth, r, main="Residual plot of Wealth vs Residuals", xlab="Wealth", ylab="Residuals")

qqnorm(r, main="Normal plot of Final Banking Model")
qqline(r)

#Influence points for final model
cat("Influence points for Final Model :\n")
print(influence.measures(m4))
summary(influence.measures(m4))

detach(banking)
#Salary Problem
#Read in Salary Survey DataFrame
salary_df <- read.table("salary-survey.txt",header=T)
head(salary_df)

exper <- salary_df$exper
educ <- salary_df$educ
mgt <- salary_df$mgt
salary <- salary_df$salary

#Create Dummy Variables
dummy_col <- as.numeric(educ == 2)
print(dummy_col)
dummy_ad <- as.numeric(educ == 3)
print(dummy_ad)

#Create Regression Model
m6 <- lm(salary ~ exper + dummy_col + dummy_ad + mgt)
summary(m6)

#6 pairwise scatter plots
plot(exper, educ, main="Scatter Plot of Experience vs Education", xlab="Experience", ylab="Education")
plot(exper, mgt, main="Scatter Plot of Experience vs Management", xlab="Experience", ylab="Management")
plot(exper, salary, main="Scatter Plot of Experience vs Salary", xlab="Experience", ylab="Salary")
plot(educ, salary, main="Scatter Plot of Education vs Salary", xlab="Education", ylab="Salary")
plot(mgt, salary, main="Scatter Plot of Management vs Salary", xlab="Management", ylab="Salary")
plot(educ, mgt, main="Scatter Plot of Education vs Management", xlab="Education", ylab="Management")

r <- residuals(m6)
p <- fitted(m6)

plot(p, r, main="Residual Plot of Salary Model", xlab="Predicted Values", ylab="Residuals")

plot(exper, r, main="Residual Plot vs Experience", xlab="Experience", ylab="Residuals")
plot(dummy_col, r, main="Residual Plot vs College", xlab="College", ylab="Residuals")
plot(dummy_ad, r, main="Residual Plot vs Advanced Degree", xlab="Advanced Degree", ylab="Residuals")
plot(mgt, r, main="Residual Plot vs Management", xlab="Management", ylab="Residuals")

qqnorm(r, main="Normal Plot of Residuals for Salary Model")
qqline(r)

#New Data Set with Example
exper2 <- c(exper, 3)
print(exper2)
dummy_col2 <- c(dummy_col, 0)
dummy_ad2 <- c(dummy_ad, 0)
mgt2 <- c(mgt, 0)
salary2 <- c(salary, NA)
#DataSet without Dependent Variable
new_data <- data.frame(exper2, dummy_col2, dummy_ad2, mgt2)
model2 <- lm(salary2 ~ exper2 + dummy_col2 + dummy_ad2+ mgt2)

pred <- predict(model2, newdata=new_data, interval="prediction")
print(pred)
