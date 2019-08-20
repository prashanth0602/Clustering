setwd("C:/Users/prashanth j/Documents/Data sets/case study")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)
library(readr)
library(cluster)    
library(factoextra) 
library(arulesViz)
library(gridExtra)
library(caret)

#data understanding
customers <- read.csv("Mall_Customers.csv")
dim(customers)
str(customers)

#Visualization of Gender
a=table(customers$Gender)
barplot(a,main="Gender Comparison Using BarPlot",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2))
install.packages("plotrix")
library(plotrix)

pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
pie3D(a,labels=lbs,
      main="Pie Chart Showing Ratio of Female and Male")

# visual Analysis of age
summary(customers$Age)
hist(customers$Age,
     col="blue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)
boxplot(customers$Age,
        col="blue",
        main="Boxplot for Descriptive Analysis of Age")

#visual Analysis of Annual income
summary(customers$Annual.Income..k..)
hist(customers$Annual.Income..k..,
     col="blue",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)
plot(density(customers$Annual.Income..k..),
     col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(customers$Annual.Income..k..),
        col="blue")

#visual Analysis of spending score
summary(customers$Spending.Score..1.100.)
boxplot(customers$Spending.Score..1.100.,
        horizontal=TRUE,
        col="blue",
        main="BoxPlot for Descriptive Analysis of Spending Score")
hist(customers$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="blue",
     labels=TRUE)

#Data Preparation for Model Building

# we will take only annual income and spending score
data <- customers[c(4,5)]

# Then, Nomalize new dataset
df <- scale(data)
df

# clustering distance measurement 
dist.eucl <- dist(df, method = "euclidean")

# Reformat as a matrix
# Subset the first 3 columns and rows and Round the values
round(as.matrix(dist.eucl)[1:3, 1:3], 1)


#determing the optimal clusters by elbow method
set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(data, k, nstart = 10)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 2:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
 
set.seed(123)
fviz_nbclust(data, kmeans, method = "wss")



set.seed(123)
k5 <- kmeans(data, centers = 5, nstart = 20)
k6 <- kmeans(data, centers = 6, nstart = 20)
k7 <- kmeans(data, centers = 7, nstart = 20)
k5

# plot for compair result 
p1 <- fviz_cluster(k5, geom = "point", data = data) + ggtitle("k = 5")
p2 <- fviz_cluster(k6, geom = "point",  data = data) + ggtitle("k = 6")
p3 <- fviz_cluster(k7, geom = "point",  data = df) + ggtitle("k = 7")

grid.arrange(p1)

customers <- customers %>% mutate(cluster = k5$cluster)
table(customers$cluster)
customers %>% select(Age,Annual.Income..k..,Spending.Score..1.100.,cluster) %>% group_by(cluster) %>% summarise_all("mean")


ggplot(data = customers ,aes(Annual.Income..k..,Spending.Score..1.100.)) + geom_point(aes(colour = factor(cluster)))
