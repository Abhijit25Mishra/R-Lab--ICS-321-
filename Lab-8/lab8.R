# Find all the files on 
# https://github.com/Abhijit25Mishra/R-Lab--ICS-321-
# https://rpubs.com/Panda_250
print("Abhijit Mishra")
set.seed(678)


print(getwd())
setwd("C:/Users/ASUS/OneDrive/Desktop/Study-Material/IIIT-Kottayam/SEM-6/Data warehousing and mining ICS 321/Lab/Lab-8")
#
#Importing the dataset
#

path <- 'titanic.csv'
titanic <-read.csv(path)

# having a view of the data
summary(titanic)
head(titanic)
tail(titanic)

# Since the data is sorted we need to randomize the data
shuffle_index <- sample(1:nrow(titanic))
head(shuffle_index)

# change the indexing i.e. make it shuffled
titanic <- titanic[shuffle_index, ]
head(titanic)
View(titanic)

#
# Cleaning the data 
#

library(dplyr)
# Drop variables
clean_titanic <- titanic %>%select(-c(Cabin, Name, PassengerId, Ticket)) %>% mutate(Pclass = factor(Pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),Survived = factor(Survived, levels = c(0, 1), labels = c('No', 'Yes')))%>%na.omit()
glimpse(clean_titanic)

#
# Create train and test data
#

# splitting the data into test and train in the given split ratio
X = sample.split(Y = clean_titanic,SplitRatio = 0.8)
traindata = clean_titanic[X,]
testdata = clean_titanic[!X,]

# viewing the new data and its dimentions
head(traindata)
dim(clean_titanic)
dim(traindata)
dim(testdata)

# used to verify the if the randomization process is correct 
prop.table(table(traindata$Survived))
prop.table(table(testdata$Survived))

#
# Building the model
#

# installing and importing libraries 
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

fit <- rpart(Survived~., data = traindata, method = 'class')
rpart.plot(fit, extra = 106)

#
# Make a Precdiction
#

# testing on the unseen i.e. the test data
predict_unseen <-predict(fit, testdata, type = 'class')
table_mat <- table(testdata$Survived, predict_unseen)

#
# Measuring performance
#

# predicted 139 entries correctly 40 entries incorrectly  
table_mat

# Printing the accuracy
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

#
# Tune the hyper-parameters
#

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, testdata, type = 'class')
  table_mat <- table(testdata$Survived, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- rpart.control(minsplit = 4,
                         minbucket = round(7 / 3),
                         maxdepth = 5,
                         cp = 0)

tune_fit <- rpart(Survived~., data = traindata, method = 'class', control = control)
accuracy_tune(tune_fit)
# after tuning the accuracy the 77% to 81%








