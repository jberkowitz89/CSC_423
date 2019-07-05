library(dplyr)
#Project 2
#Create and print a SAS dataset or R dataframe named flour. 
flour <- read.table("flour.txt", header=T)
head(flour)

#Use SAS or R to compute the means and standard deviations for weight and nbags. 
#Also compute the correlation between weight and nbags.
mean(flour$Weight)
mean(flour$NBags)

sd(flour$Weight)
sd(flour$NBags)

cor(flour$Weight, flour$NBags)

#Use SAS or R to find the simple linear regression model for predicting 
#nbags from weight. Compare your hand calculations in Question A3 to the 
#simple linear regression model obtained by SAS or R.

m1 <- glm(flour$NBags ~ flour$Weight, data=flour)
m1

#For the simple linear regression model, create and interpret the 
#residual plot and normal plot of the residuals.
r = residuals(m1)
p = fitted(m1)
plot(p, r, main="Residual Plot for the Flour Dataset",
     xlab="Predicted Values",
     ylab="Residuals")

#Create Normal Plot of the residuals
qqnorm(r, main="Normal Plot of Residuals for Flour Dataset")
qqline(r)

#Use SAS or R to find the regression through the origin model for 
#predicting nbags from weight. Compare your hand calculation in 
#Question B6 to the regression through the origin model obtained with SAS or R
m2 <- glm(NBags ~ Weight + 0, data = flour)
m2

#For the regression through the origin model, create and 
#interpret the residual plot and normal plot of the residuals.
r = residuals(m2)
p = fitted(m2)
plot(p, r, main="Residual plot for the Flour Dataset, model with 0 origin",
     xlab="Predicted Values", ylab="Residuals")

qqnorm(r, main="Normal Plot of Residuals of 0 Origin Model")
qqline(r)

#Problem 2
#Collect the following used car data from the internet or elsewhere for at 
#least 20 cars (or other vehicles like motorcycles or motorboats) of the 
#same make and model: price, year, miles.

used_cars <- read.table("UsedCars.txt", header=T)
head(used_cars)
options(scipen=999)
#Create the pairwise scatterplots of year, miles, and price. 
plot(used_cars$Year, used_cars$Miles, main="ScatterPlot of Years & Miles",
     xlab="Year", ylab="Miles")

plot(used_cars$Year, used_cars$Price, main="Scatterplot of Year and Price",
     xlab="Year", ylab="Price")

plot(used_cars$Miles, used_cars$Price, main="Scatterplot of Miles and Price",
     xlab="Miles", ylab="Price")

#Find the pairwise correlations of year, miles, and price with SAS or R. 
#Interpret them.

print("Correlation between Year and Miles")
cor(used_cars$Year, used_cars$Miles)

cor(used_cars$Year, used_cars$Price)

cor(used_cars$Miles, used_cars$Price)

#Find the simple linear regression model price=year with SAS or R.
m2 <- glm(Price ~ Year, data = used_cars)  
m2

#Create residual plot of price-year model
r <- residuals(m2)
p <- fitted(m2)
plot(p, r, main="Residual Plot of Price-Year",
     xlab="Predicted Values",
     ylab="Residuals")

#Create normal plot of the residuals of price-year model
qqnorm(r, main = "Normal Plot of residuals of Price-Year")
qqline(r)

#Simple linear regression of price-miles
m3 <- glm(Price ~ Miles, data = used_cars)
print(summary(m3))

#Residual Plot of price-miles model
r2 <- residuals(m3)
p2 <- fitted(m3)
plot(p2, r2, main="Residual Plot of Price-Miles",
     xlab="Predicted Values",
     ylab="Residuals")

#Normal plot of price-miles model
qqnorm(r2, main ="Normal Plot of residuals of Price-Model")
qqline(r2)


#Multiple Regression Model of price-years, miles
m4 <- lm(Price ~ Year + Miles, data = used_cars)
print(summary(m4))

#Find Residuals vs Predicted Values of Multiple regression Model
r3 <- residuals(m4)
p3 <- fitted(m4)
plot(p3, r3, main="Residual Plot of Price - Years Miles Model",
     xlab="Predicted Values",
     ylab="Residuals")

#Find Residuals vs year
  plot(used_cars$Year, r3, main="Residual Plot of Residuals vs Year",
       xlab="Year",
       ylab="Residuals")

#Find Residuals vs miles
plot(used_cars$Miles, r3, main="Residual Plot of Residuals vs Miles",
     xlab="Miles",
     ylab="Residuals")

#Normal Plot of Multiple regression Model
qqnorm(r3, main="Normal Plot of residuals of Price - Years Miles Model")
qqline(r3)

