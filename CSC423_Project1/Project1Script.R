#Problem 1
#Read in the data from paper1.txt and print it to verify that 
#everything was input correctly. 
pap1 <- read.table("paper1.txt", header = T)
print(pap1)

#Read in the data from paper2.txt and print it to 
#verify that everything was input correctly. 
pap2 <- read.table("paper2.txt", header = T)
print(pap2)

#Obtain these univariate statistics separately by color for the 
#paper thicknesses: sample mean, sample standard deviation, sample median, 
#sample IQR, these percentiles: 5, 10, 25, 75, 90, 95.  You can use the 
#SAS proc means or proc univariate to compute these statistics. 
#Don't compute them by hand.
cat("Mean Paper Brand A:\n")
mean(pap1$A)

cat("Mean Paper Brand B:\n")
mean(pap1$B)

cat("Standard Deviation Paper Brand A:\n")
sd(pap1$A)

cat("Standard Deviation Paper Brand B:\n")
sd(pap1$B)

cat("Median of Paper Brand A:\n")
median(pap1$A)
cat("Median of Paper Brand B:\n")
median(pap1$B)

cat("IQR of Paper Brand A:\n")
IQR(pap1$A)
cat("IQR of Paper Brand B:\n")
IQR(pap1$B)

cat("5th, 10th, 25th, 75th, 90th, 95th Percentiles of Paper Brand A:\n")
quantile(pap1$A, c(0.05, 0.10, 0.25, 0.75, 0.90, 0.95))
cat("5th, 10th, 25th, 75th, 90th, 95th Percentiles of Paper Brand B:\n")
quantile(pap1$B, c(0.05, 0.10, 0.25, 0.75, 0.90, 0.95))

#Find 95% confidence intervals for the true thickness for each color of paper 
#separately.  Show your hand calculations and also 
#show the relevent SAS or R output to verify your calculations.
error <- qt(0.975, 21)*sd(pap1$A)/sqrt(22)
print(error)
left <- mean(pap1$A-error)
print(left)
right <- mean(pap1$A+error)
print(right)

error <- qt(0.975, 21)*sd(pap1$B)/sqrt(22)
print(error)
left <- mean(pap1$B-error)
print(left)
right <- mean(pap1$B+error)
print(right)

#Create three histograms for thicknesses from the combined colors of paper:
#Create a histogram using the default setting for the number of bins 
#Run your SAS or R code first without the code in steps 6b or 6c to 
#see what bins are obtained with the default setting. 

#Create a histogram with more bins than the default. In SAS, you can do this 
#with an option on the histogram statement of proc univariate. For example:
#histogram / endpoints = (11 to 25 by 1);
#11 is a value that is less than all of the data values and 25 is a value
#that is (greater than all of the data values. 1.0 is the width of the 
#histogram bins. (You will use different numbers that make sense for your
#histograms.)
                                                                                  
#In R, you can set the number of bin boundaries (breaks) like this:
#hist(x, breaks=seq(11, 25, 1)                                          
#Create a histogram with less bins than the default. (See step 6b.)
                                                                                  
hist(pap2$thickness)
hist(pap2$thickness, breaks=seq(.05, .2, .01))

min(pap2$thickness)
max(pap2$thickness)

hist(pap2$thickness, breaks=seq(.05,.2,.05))

#Create side-by-side boxplots of the thicknesses for the colors white and yellow.  
#Discuss what the boxplots tell you. Are there any outliers? If you are using SAS, use the paper2.txt dataset and 
#sort the dataset by color before plotting the boxplots.
boxplot(pap1$A)
boxplot(pap1$B)

boxplot(pap1$A, pap1$B, names = c("Paper A", "Paper B"))

#Problem 2
n_users = c(scan( ))
17.2  22.1  18.5  17.2  18.6  14.8  21.7  15.8  16.3  22.8
24.1  13.3  16.2  17.5  19.0  23.9  14.8  22.2  21.7  20.7
13.5  15.8  13.1  16.1  21.9  23.9  19.3  12.0  19.9  19.4
15.4  16.7  19.5  16.2  16.9  17.1  20.2  13.4  19.8  17.7
19.7  18.7  17.6  15.9  15.2  17.1  15.0  18.8  21.6  11.9

print(n_users)

#Create create and interpret the normal plot for nusers. 
qqnorm(n_users)
qqline(n_users)

#Compute a 95% confidence interval for nusers.
mean(n_users)
t.test(n_users, y=NULL, mu = 17.2)

#Compute a 95% confidence interval for nusers. Show your hand calculations 
#with the relevant SAS or R output. Don't use the standard normal confidence
#interval [-1.96,1.96], use the t-distribution confidence interval obtained
#from the t-table for 50 - 1 = 49 degrees of freedom. You can check your 
#answer with SAS using proc means or proc ttest. You can check your answer
#with R using the t.test function.

qt(0.975, 49)

#Problem 3
#If you are using SAS, create labels for each variable thickness and color. 
#If you are using R, add print statements in your source code to explain 
#what your output means. 

pap1clean <- read.table("paper1-cleaned.txt", header = T)
print(pap1clean)

pap2clean <- read.table("paper2-cleaned.txt", header = T)
print(pap2clean)

thickAClean <- c(pap1clean$A)
print(thickAClean)

thickBClean <-c(pap1clean$B)
print(thickBClean)

#Create normal plots of the thicknesses separately for the paper 
#colors white and yellow. Interpret these normal plots. 
qqnorm(thickAClean)
qqline(thickAClean)

qqnorm(thickBClean)
qqline(thickBClean)

#Type out the five steps of a 0.05-level paired-sample t-test to test 
#the null hypothesis that there is no difference between the paper 
#thicknesses in paper1.txt. Show relevent SAS or R output in your report. 
#You will need to obtain the confidence interval for the test statistic 
#from the t-table.

t.test(thickAClean, thickBClean, alternative="two.sided",
       paired=TRUE, conf.level=0.95)