# Find all the files on 
# https://github.com/Abhijit25Mishra/R-Lab--ICS-321-
# https://rpubs.com/Panda_250
print("Abhijit Mishra")

#Find Sum, Mean and Product of a Vector in R
vec1<-c(1.1, 2, 3.0, 4.2)
sum(vec1)
prod(vec1)
mean(vec1)

vec2<-c(1.1,NA, 2, 3.0,NA )
sum(vec2)
prod(vec2)
mean(vec2)

library(dplyr)
library(stringr)

#Count Number of Occurrences of Certain Character in String in R
str = c("$I%Love!R programming %","cs^e%portal", "le%.5%rty.in","join2022%")
print(str)
str_count(str,pattern = "%")
str_count(str,pattern = 'r')


#Write a R program to create an 3 dimensional array of 24 elements using the dim() function.
ar <- array(1:5,c(2,3,4))
ar
dim(ar)

# Write a R program to print the numbers from 1 to 100 and print "Buy" for multiples of
# 4, print "Now" for multiples of 5, and print "BuyNow" for multiples of both.
for(i in 1:100){
  if(i%%4==0 && i%%5==0){
    print("BuyNow")
  }
  else if(i%%4==0){
    print("Now")
  }
  else if(i%%5==0){
    print("Buy")
  }
  else{
    print(i)
  }
}
# Write a R program to create the system's current date with and without time
print(Sys.Date())
print(Sys.time())

# Task will be based on mtcars dataset. This dataset consists of information regarding car
#characteristics like weight, fuel consumption, transmission type etc. Solve thefollowing
#queries in R using Select, Filter, Mutate, Summarise and Arrange functions.
data<-mtcars
View(data)
dim(data)
summary(data)

#  Select random n rows.
slice_sample(data,n=5)
# Select random fraction of rows
sample_frac(data,n=2)
# Select cyl, hp, and wt columns from the mtcars dataset.
data %>% select(cyl, hp, wt )
# Filter the data by taking only cars that have an automatic transmission.
filter(mtcars, am == 1)
# Retrieve cars with horsepower larger than 225.
filter(mtcars,hp>225)
# Calculate the mean & median for the variable mpg.
mean(mtcars$mpg)
median(mtcars$mpg)
# Sort the variable hp in descending order.
arrange(mtcars,desc(hp))


# Read the following data into R and give the resulting dataframe a name 
days <- c("Monday","Tuesday","Wednesday","Thrusday","Friday","Saturdat","Sunday")
nrbirds <- c(2,5,0,8,1,2,3)
df<-data.frame(days,nrbirds)
# Add a day number to the dataset you read in above (Monday=1,sunday=7).
x<-c(1,2,3,4,5,6,7)
# Delete the ’Day’ variable from the dataset and only keep the daynumber that you addedin the above question.
df$dayNumber <- x
df = subset(df,select = -c(days))
df

# On which daynumber did you observe the most number of birds?
max_row <- which.max(df$nrbirds)
print(max_row)
most_birds_daynumber <- df$dayNumber[max_row]
print(most_birds_daynumber)
# Sort the dataset by number of birds seen.
df <- arrange(df, desc(nrbirds))
df
