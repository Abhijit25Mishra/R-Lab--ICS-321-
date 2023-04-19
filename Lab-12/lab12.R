# Find all the files on 
# https://github.com/Abhijit25Mishra/R-Lab--ICS-321-
# https://rpubs.com/Panda_250
print("Abhijit Mishra")

# taking in input
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# performing linear regression
relation <- lm(y~x)

relation
summary(relation)

# predecting ouput using the linear regesrrion relation we created earlier
a <- data.frame(x = 170)
result <-  predict(relation,a)
print(result)

# Give the chart file a name.
png(file = "linearregression.png")

# Plot the chart.
plot(y,x,col = "blue",main = "Height & Weight Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")

dev.off()

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

# setting the working directory to use the csv file
print(getwd())
setwd("C:/Users/ASUS/OneDrive/Desktop/Study-Material/IIIT-Kottayam/SEM-6/Data warehousing and mining ICS 321/Lab/Lab-12")


income = read.csv("incomedata.csv")
heart = read.csv("heartdata.csv")
summary(income)
summary(heart)

hist(income$happiness)
plot(happiness ~ income, data = income)
cor(heart$biking, heart$smoking)
hist(heart$heart.disease)

plot(heart.disease ~ biking, data=heart)
plot(heart.disease ~ smoking, data=heart)

income.happiness.lm <- lm(happiness ~ income, data = income)

summary(income.happiness.lm)

heart.disease.lm<-lm(heart.disease ~ biking + smoking, data = heart)

summary(heart.disease.lm)


par(mfrow=c(2,2))
plot(income.happiness.lm)
par(mfrow=c(1,1))


par(mfrow=c(2,2))
plot(heart.disease.lm)
par(mfrow=c(1,1))

income.graph<-ggplot(income, aes(x=income, y=happiness))+geom_point()
income.graph

income.graph <- income.graph + geom_smooth(method="lm", col="black")
income.graph

income.graph <- income.graph +stat_regline_equation(label.x = 3, label.y = 7)
income.graph


income.graph + theme_bw() + labs(
       title = "Reported happiness as a function of income",
       x = "Income (x$10,000)",
       y = "Happiness score (0 to 10)"
       )

plotting.data<-expand.grid(
  biking = seq(min(heart$biking), max(heart$biking), length.out=30),
  smoking=c(min(heart$smoking), mean(heart$smoking), max(heart$smoking)))

plotting.data$predicted.y <- predict.lm(heart.disease.lm, newdata=plotting.data)

plotting.data$smoking <- round(plotting.data$smoking, digits = 2)

plotting.data$smoking <- as.factor(plotting.data$smoking)

heart.plot <- ggplot(heart, aes(x=biking, y=heart.disease)) +
  geom_point()

heart.plot

heart.plot <- heart.plot +
  geom_line(data=plotting.data, aes(x=biking, y=predicted.y, color=smoking))

heart.plot


heart.plot <-
  heart.plot +
  theme_bw() +
  labs(title = "Rates of heart disease (% of population) \n as a function of biking to work and smoking",
       x = "Biking to work (% of population)",
       y = "Heart disease (% of population)",
       color = "Smoking \n (% of population)")

heart.plot

heart.plot + annotate(geom="text", x=30, y=1.75, label=" = 15 + (-0.2*biking) + (0.178*smoking)")

print("We found a significant relationship between income and happiness (p < 0.001, R2 = 0.73 ± 0.0193), with a 0.73-unit increase in reported happiness for every $10,000 increase in income.")


