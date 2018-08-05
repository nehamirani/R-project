#1. Import train.csv file from Titanic_dataset.
setwd("C:/Users/Neha/Desktop/Data Analytics/Project2/Titanic Dataset")
getwd()

train<-read.csv("train.csv")

#Exploratory Analysis.
View(train)
summary(train)

#Factors and Levels
#A. Find number of Passengers according to their Group Class: 1st , 2nd , 3rd

train.pclass <- as.factor(train$Pclass)
print("Number of passengers according to their Group Class:")
summary(train.pclass)

#B. Find number of Passengers according to their Group Sex: Male, Female.

train.gender <- as.factor(train$Sex)
print("Number of passengers according to their Group Sex: Male, Female:")
summary(train.gender)

#Find stats of Passengers Age.
train.age <- as.factor(train$Age)
print("Number of passengers according to their Age:")
summary(train.age)

##check if age is less than one
Age<-subset(train,Age<1)
print(Age)

##There are records present where Age is less than one and the value is fractional

##D. Find number of Passengers according to their Group Embarked: Place where the passenger embarked their journey. One of Cherbourg, Queenstown or Southampton.
train.emb <- as.factor(train$Embarked)
print("Number of passengers according to their Group Embarked:")
summary(train.emb)

##3. Response Variables
##A. Validate number of passengers who survived / Not Survived
train.sur <- as.factor(train$Survived)
print("Number of passengers who survived / Not Survived:")
summary(train.sur)

##4. Exploratory Data Analysis: 
##A. Explore / Print first n Records from Dataset.
head(train)

##B. Find mean, median, quartile, max, min data for every feature.
summary(train)

##slicing the dataset for required features
train_df<-train[,c(2,3,5,6,12)]
print(train_df)
View(train_df)

##D. Perform data cleaning steps

train_df=na.omit(train_df)
rownames(train_df)<-1:nrow(train_df)
View(train_df)

##E. Encode Data

train_df$Age=ifelse(train_df$Age<= 18,"child",ifelse(train_df$Age>60,"adult","senior"))
train_df$Age=as.factor(train_df$Age)

##Validate the above step
print(head(train_df$Age))

##A. Plot the barplot of all four input variables:
barplot(table(train_df$Pclass),xlab="Pclass",ylab="Frequency",main="Pclass Barplot")
barplot(table(train_df$Sex),xlab="Gender",ylab="Frequency",main="Gender Barplot")
barplot(table(train_df$Age),xlab="Age",ylab="Frequency",main="Age Barplot")
barplot(table(train_df$Embarked),xlab="Embarked",ylab="Frequency",main="Embarked Barplot")

##B. Convert the categorical dataframe into numeric dataframe.
train_df$Survived=as.integer(train_df$Survived)
train_df$Pclass=as.integer(train_df$Pclass)
train_df$Sex=as.integer(train_df$Sex)
train_df$Age=as.integer(train_df$Age)
train_df$Embarked=as.integer(train_df$Embarked)

##6. Statistical Analysis:
##A. Number of survivors on an average from Class & Plot a scatter plot
avg_pclass=c(0,0,0)
avg_pclass[1]=mean(train_df$Survived[train_df$Pclass==1])
avg_pclass[2]=mean(train_df$Survived[train_df$Pclass==2])
avg_pclass[3]=mean(train_df$Survived[train_df$Pclass==3])
print(avg_pclass)

plot(avg_pclass,type="o",xaxt="n",main="Mean of individual pclass",xlab="Pclass",ylab="Mean")
axis(1,at=c(1,2,3),labels = c("1","2","3"))

View(train_df)

##B. Number of survivors on an average from Gender & Plot a scatter plot
avg_gender=c(0,0)
avg_gender[1]=mean(train_df$Survived[train_df$Sex==1])
avg_gender[2]=mean(train_df$Survived[train_df$Sex==2])
print(avg_gender)

plot(avg_gender,type="o",xaxt="n",main="Mean of individual gender",xlab="Gender",ylab="Mean")
axis(1,at=c(1,2),labels = c("Female","Male"))

##C. Number of survivors on an average from Every Port of Embarkment & Plot a scatter plot.
avg_emb=c(0,0,0)
avg_emb[1]=mean(train_df$Survived[train_df$Embarked==4])
avg_emb[2]=mean(train_df$Survived[train_df$Embarked==2])
avg_emb[3]=mean(train_df$Survived[train_df$Embarked==3])
print(avg_emb)

plot(avg_emb,type="o",xaxt="n",main="Mean of individual Embarkment",xlab="Embarkment",ylab="Mean")
axis(1,at=c(1,2,3),labels = c("Southampton","Cherbourg","Queenstown"))

#Validate above scatterplots using ANOVA ( 1 way Interaction )
v1=aov(train_df$Survived ~ train_df$Pclass)
anova(v1)

v2=aov(train_df$Survived ~ train_df$Sex)
anova(v2)

v3=aov(train_df$Survived ~ train_df$Embarked)
anova(v3)
