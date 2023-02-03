# Find all the files on 
# https://github.com/Abhijit25Mishra/R-Lab--ICS-321-
# https://rpubs.com/Panda_250
print("Abhijit Mishra")



# setting the working directory to use the csv file
print(getwd())
setwd("C:/Users/ASUS/OneDrive/Desktop/Study-Material/IIIT-Kottayam/SEM-6/Data warehousing and mining ICS 321/Lab/Lab-4")

# reading the csv file
sdata <- read.csv("Dataset.csv")

# Viewing the data
head(sdata)
which(is.na(sdata))
View(sdata)

# Handling the missing data
sdata$Age = ifelse(is.na(sdata$Age),ave(sdata$Age, FUN = function (x)mean(x, na.rm = TRUE)),sdata$Age)
sdata$Salary = ifelse(is.na(sdata$Salary),ave(sdata$Salary,FUN = function (x)mean(x,na.rm=TRUE)),sdata$Age)

# Encoding the data categorically
#sdata$Country = factor(sdata$Country, levels = c('France','Spain','Germany'), labels = c(1.0, 2.0 , 3.0 ))
sdata$Purchased = factor(sdata$Purchased,levels = c('No', 'Yes'),labels = c(0, 1))
sdata$Purchased[is.na(sdata$Purchased)] <- 0
as.factor(sdata$Purchased)

# splitting the dataset into the training and test set
library(caTools)
set.seed(123)
split = sample.split(sdata$Purchased,SplitRatio = 0.2)
training_set = subset(sdata,split == FALSE)
test_set = subset(sdata,split == TRUE)
View(training_set)
View(test_set)
training_set[, 2:3] = scale(training_set[, 2:3])
test_set[, 2:3] = scale(test_set[, 2:3])
training_set
test_set




# importing plotting lib
require(ggplot2)

# Viewing the data
View(iris)

# number of attributes
attributes(iris)

# Structure of data
typeof(iris)
str(iris)

# Size of data
ncol(iris)
nrow(iris)
dim(iris)
length(iris)

# summary of dataset
summary(iris)

# First row and last of dataset 
head(iris,1)
tail(iris,1)

# First 10 values of sepal length
head(iris[1],10)

# mean, median, range and quartiles are given in the summary
summary(iris)
summary(iris$Sepal.Length)

# variance of iris
var(iris[0:4])

# Histogram
hist(iris$Petal.Length,breaks = 30,col="blue", xlab = "Petal Length", main = " Histogram of Pertal Length" )
qplot(Sepal.Length, data=iris, geom='histogram', fill=Species, alpha=I(1/2))

# Density plot
qplot(Petal.Length, data=iris, geom='density')
qplot(Petal.Length, data=iris, geom='density', color=Species, fill=Species)

# Scatter plot
qplot(Petal.Width, Petal.Length, data=iris, color=Species)
# correlation and covariance
cor(iris$Sepal.Length, iris$Petal.Length)
cov(iris$Sepal.Length, iris$Petal.Length)
cov(iris[,1:4])

# making every plot i know of ;)
df <- iris[, 1:4]
boxplot(df)
pairs(df)
stars(df)
boxplot(iris)
scatter.smooth(iris)
table(iris)
pie(table(iris))
PL <- iris$Petal.Length
barplot(PL)
hist(PL)
SP <- iris$Species
boxplot(PL ~ SP)
summary(aov(PL ~ SP))
PW <- iris$Petal.Width
plot(PL, PW, col = SP)
abline(lm(PW ~ PL))
ma <- as.matrix(iris[, 1:4]) # convert to matrix
disMatarix <- dist(ma)
plot(hclust(disMatarix))
heatmap(ma,scale = "column",RowSideColors = rainbow(3)[iris$Species])




# inputting the age and %fat
age <- c(23,23,27,27,39,41,47,49,50,52,54,54,56,57,58,58,60,61)
fatpercent <- c(9.5,26.5,7.8,17.8,31.4,25.9,27.4,27.2,31.2,34.6,42.5,28.8,33.4,30.2,34.1,32.9,41.2,35.7)

# converting the data into dataframes
data <- list(Age = age,FatPercent = fatpercent)
data
df <- as.data.frame(data)
df

# finding the correlation
cor(df$Age,df$FatPercent)


