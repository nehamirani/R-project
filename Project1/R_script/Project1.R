#Import iris.csv file from folder iris_dataset.
setwd("C:/Users/Neha/Desktop/Data Analytics/Project1/iris_Dataset")
getwd()

iris<-read.csv("iris.csv",stringsAsFactors = F)

#Exploratory Analysis.
View(iris)

#A. Explore / Print first 3 Records from Dataset.

print(head(iris,3))

#B. Find Dimension of Dataset.

print(dim(iris))
# The dimensions are 150 rows and 5 columns

#C. Find Names , Class of features in the Dataset.

print(names(iris))
# The names of features are "sepal_length" "sepal_width"  "petal_length" "petal_width"  "species" 

print(class(iris))
# The class of the iris dataset is data.frame

print(class(iris$sepal_length))
print(class(iris$sepal_width))
print(class(iris$petal_length))
print(class(iris$petal_width))
print(class(iris$species))
# The class of species is character, rest other features are of numeric data type

#D. Find missing values (if any) & make the data consistent by removing it.
colSums(is.na(iris)) ##There are no missing values

#E. Find Structure of Data.
str(iris)

#F. Find mean, median, quartile, max, min data for every feature.
print(summary(iris))
#sepal_length - Min.:4.3,Max.:7.9,Mean:5.843,Median:5.8,1st Quantile:5.1,3rd Quantile:6.4
#sepal_width - Min.:2,Max.:4.4,Mean:3.054,Median:3,1st Quantile:2.8,3rd Quantile:3.3
#petal_length - Min.:1,Max.:6.9,Mean:3.759,Median:4.35,1st Quantile:1.6,3rd Quantile:5.1
#petal_width - Min.:0.1,Max.:2.5,Mean:1.199,Median:1.3,1st Quantile:0.3,3rd Quantile:1.8

#G. Plot a Boxplot Graph, Pie chart respective to their Species.

species_setosa<-subset(iris,iris$species=='setosa')
boxplot(species_setosa[1:4],xlab="Features",ylab="Scale", main="Boxplot of features for setosa")

species_versicolor<-subset(iris,iris$species=='versicolor')
boxplot(species_versicolor[1:4],xlab="Features",ylab="Scale", main="Boxplot of features for versicolor")

species_virginica<-subset(iris,iris$species=='virginica')
boxplot(species_virginica[1:4],xlab="Features",ylab="Scale", main="Boxplot of features for virginica")

#Pie chart
cols=c('red','green','blue')
pie(table(iris$species),col=cols,main="Species Frequency")

##There are equal number of records for each species type

#H. Subset tuples based on their Species in different R-Object.
species_1<- subset(iris, iris$species=='setosa')
print(species_1)

species_2<- subset(iris, iris$species=='versicolor')
print(species_2)

species_3<- subset(iris, iris$species=='virginica')
print(species_3)

#I. Plot a BoxPlot Graph for Individual R-Object.
boxplot(iris$sepal_length)
boxplot(iris$sepal_width)
boxplot(iris$petal_length)
boxplot(iris$petal_width)

#J. Plot a Histogram on feature Petal lengths of iris dataset .
hist(iris$petal_length,main="Histogram for feature Petal length",col="Green",xlab="Petal Length")

#K. Plot a Histogram for Petal Lengths of Different Species on different Graph.
hist(species_1$petal_length,main="Histogram of feature Petal length for setosa species",col="Orange",xlab="Petal Length")
hist(species_2$petal_length,main="Histogram of feature Petal length for versicolor species",col="Orange",xlab="Petal Length")
hist(species_3$petal_length,main="Histogram of feature Petal length for virginica species",col="Orange",xlab="Petal Length")

#L. Find correlation between multiple features also plot a scatter plot for correlation.
corr_iris<-cor(iris[1],iris[2])
print(corr_iris)

##pairs(iris[1:4],pch=21,bg=c("red","green","blue")[unclass(iris$species)],main="Iris Scatterplot")
plot(x=iris$sepal_length,y=iris$sepal_width,xlab="Sepal Length",ylab="Sepal Width",main="Sepal Length vs. Sepal Width")

#plot a Decision Tree.
library(rpart) 
install.packages("rattle")
library(rattle)

tree <- rpart(iris$species ~.,data = iris, method = 'class')
fancyRpartPlot(tree,main="Classification of Iris data based on Species")
