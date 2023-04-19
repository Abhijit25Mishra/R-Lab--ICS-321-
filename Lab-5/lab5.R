# Find all the files on 
# https://github.com/Abhijit25Mishra/R-Lab--ICS-321-
# https://rpubs.com/Panda_250
print("Abhijit Mishra")


# setting the working directory to use the csv file
print(getwd())
setwd("C:/Users/ASUS/OneDrive/Desktop/Study-Material/IIIT-Kottayam/SEM-6/Data warehousing and mining ICS 321/Lab/Lab-5")

# reading the csv file
sdata <- read.csv("Sampledata_L6.csv")

# Familizare with the data
head(sdata)
which(is.na(sdata))
View(sdata)

# summary of data and num of rows and col and summary of ever col
sapply(sdata, class)
dim(sdata)
summary(sdata)
ncol(sdata)
nrow(sdata)
summary(sdata)
lapply(sdata,summary)

# making a dataframe with only the first 10 elements in the given dataset
df1<-data.frame(head(sdata,10))

names(df1)
df1 = rename(df1,Employeeno = How.many.employees.does.your.company.or.organization.have.)
View(df1)
str(df1)



df1$Are.you.self.employed. <- as.character(df1$Are.you.self.employed.)
df1$Are.you.self.employed.[df1$Are.you.self.employed.=="1"] <- "YES" #replace 1 with YES
df1$Are.you.self.employed.[df1$Are.you.self.employed.=="0"] <- "NO" #replace 0 with NO

df1$Is.your.employer.primarily.a.tech.company.organization. <- as.character(df1$Is.your.employer.primarily.a.tech.company.organization.)
df1$Is.your.employer.primarily.a.tech.company.organization.[df1$Is.your.employer.primarily.a.tech.company.organization.=="1"] <- "YES" #replace 1 with YES
df1$Is.your.employer.primarily.a.tech.company.organization.[df1$Is.your.employer.primarily.a.tech.company.organization.=="0"] <- "NO" #replace 0 with NO

df1$Is.your.primary.role.within.your.company.related.to.tech.IT. <- as.character(df1$Is.your.primary.role.within.your.company.related.to.tech.IT.)
df1$Is.your.primary.role.within.your.company.related.to.tech.IT.[df1$Is.your.primary.role.within.your.company.related.to.tech.IT.=="1"] <- "YES" #replace 1 with YES
sapply(df1, class)
df1


df1 <- df[1:50, c("What.is.your.gender.")]
unique(df[1:50, c("What.is.your.gender.")])


df1 <- gsub("I identify as female", "Female", df1)
df1 <- gsub("Bigender", "Male", df1)
df1 <- gsub("non-binary", "Male", df1)
df1 <- gsub("Female assigned at birth", "Female", df1)
df1 <- gsub("(?i)F | (?i)Female", "1", df1)
df1 <- gsub("(?i)M | (?i)Male", "0", df1)
df1 <- gsub("1", "Female", df1)
df1 <- gsub("0", "Male", df1)
df1

#Finding Missing values and replacing them with NA
df2 <- df[1:10, c(1:10)]
df2[df2==''] <- NA
df2
sum(is.na(df2))

df2 <- df[1:30, c("Are.you.self.employed.",
                  "Is.your.employer.primarily.a.tech.company.organization.",
                  "Is.your.primary.role.within.your.company.related.to.tech.IT.",
                  "What.is.your.age.")]
print("Null Values before modification")
sum(is.na(df2))
df2[is.na(df2)] <- 0
print("Null values after modification")
sum(is.na(df2))
df2

age <- df2$What.is.your.age.
hist(age, col="grey")
boxplot(df2$What.is.your.age.)

df3<-head(df$If.yes..what.condition.s..have.you.been.diagnosed.with. ,10)
df3
df4<-head(df$If.maybe..what.condition.s..do.you.believe.you.have.,10)
df4
boxplot(unique(df3))
boxplot(unique(df4))
