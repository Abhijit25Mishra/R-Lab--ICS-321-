RNGkind(sample.kind = "Rounding")
set.seed(123)

# setting the working directory to use the csv file
print(getwd())
setwd("C:/Users/ASUS/OneDrive/Desktop/Study-Material/IIIT-Kottayam/SEM-6/Data warehousing and mining ICS 321/Lab/Lab-9")


#Reading 
diabetes_df <- read.csv("diabetes.csv", header = TRUE, stringsAsFactors = T) 

#Viewing and exploring
head(diabetes_df)
names(diabetes_df) <- c("pregnancies", "glucose", "bloodpressure", "skinthickness", "insulin", "bmi", "diabetespedigreefunction", "age", "outcome")
diabetes_df$outcome <- as.factor(diabetes_df$outcome) %>% na.omit()

#Change Outcome variables as categorical with level = True (1) or False (0)
diabetes_df$outcome <- factor(diabetes_df$outcome, levels = c(0,1), labels = c("False", "True"))
glimpse(diabetes_df)
head(diabetes_df)


#Dive deeper into the structure of the data
str(diabetes_df)
describe(diabetes_df)
colSums(is.na(diabetes_df))
sum(is.na(diabetes_df))
anyNA(diabetes_df)

#Cleaning
#Replace '0' values into NA
diabetes_df[, 2:7][diabetes_df[, 2:7] == 0] <- NA

#Missing value in our dataset
diabetes_missmap <- missmap(diabetes_df)


#predict the missing values
diabetes_MICE <- mice(diabetes_df[, c("glucose","bloodpressure",
                                      "skinthickness","insulin","bmi")], 
                      method='rf')


diabetes_result <- complete(diabetes_MICE)


#add the predicted missing values into our data set
diabetes_df$glucose <- diabetes_result$glucose
diabetes_df$bloodpressure <- diabetes_result$bloodpressure  
diabetes_df$skinthickness <- diabetes_result$skinthickness  
diabetes_df$insulin  <- diabetes_result$insulin  
diabetes_df$bmi <- diabetes_result$bmi

diabetes_missmap <- missmap(diabetes_df)

#Building a model
#split data into training and test data sets
sliced <- createDataPartition(y = diabetes_df$outcome,p = 0.75,list = FALSE)

diabetes_train <- diabetes_df[sliced,]
diabetes_test <- diabetes_df[-sliced,] 

#Check dimensions of the split 
prop.table(table(diabetes_df$outcome)) * 100
prop.table(table(diabetes_train$outcome)) * 100
prop.table(table(diabetes_test$outcome)) * 100

#create x_diabetes which holds the predictor variables and y which holds the response variables
x_diabetes = diabetes_train[,-9]
y_diabetes = diabetes_train$outcome


#Our Naive Bayes Model
diabetes_NB_model = train(x_diabetes,y_diabetes,'nb',trControl=trainControl(method='cv',number=10))
diabetes_NB_model 

#Evaluation
diabetes_pred_NB <- predict(diabetes_NB_model, newdata = diabetes_test)
confusionMatrix(diabetes_pred_NB, reference = diabetes_test$outcome, positive =  "True")
