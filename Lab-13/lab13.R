# Find all the files on 
# https://github.com/Abhijit25Mishra/R-Lab--ICS-321-
# https://rpubs.com/Panda_250
print("Abhijit Mishra")

vector1 <- c(1, 1.5, 3, 5, 3.5, 4.5, 3.5)
vector2 <- c(1, 2, 4, 7, 5, 5, 4.5)

dataPoints<- array(c(vector1, vector2), dim = c(7, 2))
plot(vector1,vector2)

k=2
vec1 = c(1,5)
vec2 = c(1,7)
centroid = array(c(1,5,1,7), dim = c(k, 2))
print(centroid)


distance_from_cluster_1 = (dataPoints[,] - centroid[1,])^2
distance_from_cluster_1 = sqrt(distance_from_cluster_1[,1] + distance_from_cluster_1[,2])
distance_from_cluster_1

distance_from_cluster_2 = (dataPoints[,] - centroid[2,])^2
distance_from_cluster_2 = sqrt(distance_from_cluster_2[,1] + distance_from_cluster_2[,2])
distance_from_cluster_2

total_distance = array(c(distance_from_cluster_1, distance_from_cluster_2), dim = c(7, 2))
c(total_distance[,1] <= total_distance[,2])
dataPoints[,1][c(total_distance[,1] <= total_distance[,2])]
mean(dataPoints[,1][c(total_distance[,1] <= total_distance[,2])])

new_centroid = centroid
# Initialize new_centroids with previous values. Previous values are used as centroids are modified at the end of the algorithm.
c1 = c(mean(dataPoints[,1][c(total_distance[,1] <= total_distance[,2])]), mean(dataPoints[,2][c(total_distance[,1] <= total_distance[,2])]))
c2 = c(mean(dataPoints[,1][!c(total_distance[,1] <= total_distance[,2])]), mean(dataPoints[,2][!c(total_distance[,1] <=total_distance[,2])]))
new_centroid[1,] = c1
new_centroid[2,] = c2
new_centroid


plot(dataPoints[,1], dataPoints[,2])
points(new_centroid[,1], new_centroid[,2],col="red")


data("USArrests")  # Load the data set
head(USArrests)

str(USArrests)
summary(USArrests)
any(is.na(USArrests))

corrplot(cor(USArrests), method = "number",
         type = "lower")


USArrests <- scale(USArrests)
dim(USArrests)

head(USArrests,n=5)

set.seed(123)
crime <- sample(1:50,10)

crime_1 <- USArrests[crime,]
head(crime_1)

dist.eucl <- dist(crime_1, method = "euclidean")
head(dist.eucl)

round(as.matrix(dist.eucl)[1:4, 1:4], 1)

fviz_dist(dist.eucl)

wss <- sapply(1:crime, 
              function(k){kmeans(USArrests, k, nstart=20,iter.max = 15                     )$tot.withinss})
plot(1:crime, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fviz_nbclust(USArrests, kmeans, method = 'wss') +
  geom_vline(xintercept = 4, linetype=5, col= "darkred")

km.res <- kmeans(USArrests, 4, nstart = 20)
km.res
km.res$totss
km.res$betweenss

df_member <- cbind(USArrests, cluster = km.res$cluster)
head(df_member,10)

fviz_cluster(km.res, data = USArrests,
             palette=c("red", "blue", "black", "darkgreen"),
             ellipse.type = "euclid",
             star.plot = T,
             repel = T,
             ggtheme = theme())
