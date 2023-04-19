# Find all the files on 
# https://github.com/Abhijit25Mishra/R-Lab--ICS-321-
# https://rpubs.com/Panda_250
print("Abhijit Mishra")

print(getwd())
setwd("C:/Users/ASUS/OneDrive/Desktop/Study-Material/IIIT-Kottayam/SEM-6/Data warehousing and mining ICS 321/Lab/Lab-7")

library(tidyverse)
library(plyr)

retail <- readxl::read_excel("Online_Retail.xlsx")

summary(retail)

#complete.cases(data) will return a logical vector indicating which rows have no missing values. Then use the vector to get only rows that are complete using retail[,].
retail <- retail[complete.cases(retail),]
#mutate function is from dplyr package. It is used to edit or add new columns to dataframe. Here Description column is being converted to factor column. '
# as.factor converts column to factor column. %>% is an operator with which you may pipe values to another function or expression
retail %>% mutate(Description = as.factor(Description))

 retail %>% mutate(Country = as.factor(Country))


#Converts character data to date. Store InvoiceDate as date in new variable
retail$Date <- as.Date(retail$InvoiceDate)

#Extract time from InvoiceDate and store in another variable
TransTime<- format(retail$InvoiceDate,"%H:%M:%S")

#Convert and edit InvoiceNo into numeric
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))


#NAs introduced by coercion

#Bind new columns TransTime and InvoiceNo into dataframe retail
cbind(retail,TransTime)


cbind(retail,InvoiceNo)


#get a glimpse of your data
glimpse(retail)

#ddply(dataframe, variables_to_be_used_to_split_data_frame,function_to_be_applied)
transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))
#The R function paste() concatenates vectors to character and separated results using collapse=[any optional charcater string ]. Here ',' is used
transactionData
#set column InvoiceNo of dataframe transactionData
transactionData$InvoiceNo <- NULL
#set column Date of dataframe transactionData
transactionData$Date <- NULL
#Rename column to items
colnames(transactionData) <- c("items")
#Show Dataframe transactionData
transactionData
write.csv(transactionData,"market_basket_transactions.csv", quote =
            FALSE, row.names = FALSE)
#transactionData: Data to be written
#"D:/Documents/market_basket.csv": location of file with file name to be written to
#quote: If TRUE it will surround character or factor column with double quotes. If FALSE nothing will be quoted
#row.names: either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.
tr <- read.transactions("market_basket_transactions.csv", sep = ",")
tr
# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'),
                  main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Re
lative Item Frequency Plot")
# Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, parameter = list(supp=0.001,
                                                  conf=0.8,maxlen=10))
inspect(association.rules[1:10])
shorter.association.rules <- apriori(tr, parameter = list(supp=0.001,
                                                          conf=0.8,maxlen=3))
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) >
                        1) # get subset rules in vector
length(subset.rules) #> 3913
subset.association.rules. <- association.rules[-subset.rules] # remove subset
rules.metal.association.rules <- apriori(tr, parameter = list(supp=0.001,
                                                        conf=0.8),appearance = list(default="lhs",rhs="METAL"))
# Here lhs=METAL because you want to find out the probability of that in how many customers buy METAL along with other items

inspect(head(metal.association.rules))
metal.association.rules <- apriori(tr, parameter = list(supp=0.001,
                                                        conf=0.8),appearance = list(lhs="METAL",default="rhs"))
# Here lhs=METAL because you want to find out the probability of that in how many customers buy METAL along with other items
inspect(head(metal.association.rules))
# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
plot(subRules)
plot(rulesObject, measure, shading, method)
plot(subRules,method="two-key plot")
#plotly_arules(subRules)
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph", engine = "htmlwidget")
saveAsGraph(head(subRules, n = 1000, by = "lift"), file = "rules.graphml")
# Filter top 20 rules with highest lift
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")
inspect(association.rules[1:10])
# Filter top 20 rules with highest lift
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")









## load mushroom data from UCI the Machine Learning Repository
url <- paste0("http://archive.ics.uci.edu/ml/",
              "machine-learning-databases/mushroom/agaricus-lepiota.data")
mushrooms <- read.csv(file = url, header = FALSE)
names(mushrooms) <- c("class", "cap-shape", "cap-surface",
                      "cap-color", "bruises", "odor", "gill-attachment", "gill-
spacing",
                      "gill-size", "gill-color", "stalk-shape", "stalk-root",
                      "stalk-surface-above-ring", "stalk-surface-below-ring",
                      "stalk-color-above-ring", "stalk-color-below-ring",
                      "veil-type", "veil-color", "ring-number", "ring-type",
                      "spore-print-color", "population", "habitat")
table(mushrooms$class, useNA="ifany")
str(mushrooms)

## find associatin rules from the mushroom dataset
rules <- apriori(mushrooms, control = list(verbose=F),
                 parameter = list(minlen=2, maxlen=5),
                 appearance = list(rhs=c("class=p", "class=e"),
                                   default="lhs"))
quality(rules) <- round(quality(rules), digits=3)
rules.sorted <- sort(rules, by="confidence")
inspect(head(rules.sorted))






