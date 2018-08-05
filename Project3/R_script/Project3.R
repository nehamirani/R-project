#1. Import Train & Test DataSet from BigMart Dataset folder.
setwd("C:/Users/Neha/Desktop/Data Analytics/Project3/BigMart Sales Dataset")
getwd()

bigmart_train<-read.csv("Train_UWu5bXk.csv",stringsAsFactors = F)
bigmart_test<-read.csv("Test_u94Q5KV.csv",stringsAsFactors = F)

View(bigmart_train)
View(bigmart_test)

#2. Check dimensions (number of row & columns) & Structure in dataset.
dim(bigmart_train)
dim(bigmart_test)

str(bigmart_train)
str(bigmart_test)

#3. Find Missing Values in the dataset.
table(is.na(bigmart_train))
table(is.na(bigmart_test))

#4. Find Missing Values according to Columns.
colSums(is.na(bigmart_train))
colSums(is.na(bigmart_test))

#5. Find Summary of DataSet & Draw Conclusions from it.
summary(bigmart_train)
summary(bigmart_test)

#6. ScatterPlots
#A. Plot a ScatterPlot using ggplot for Item_Visibility vs Item_Outlet_Sales & draw conclusion from which products visibility is more sales.
install.packages("ggplot2")
library(ggplot2)

ggplot()+geom_point(aes(x=bigmart_train$Item_Visibility,y=bigmart_train$Item_Outlet_Sales),colour='blue')+ggtitle('Visibility vs. Outlet Sales')+xlab('Visibility')+ylab('Outlet Sales')

vs_sales<-recordPlot()

vs_sales
##Conclusion:- There are more sales where the product visibility has a lower value

#B. Plot a Barplot using ggplot for Outlet_Identifier vs Item_Outlet_Sales & Draw conclusion who has contributed to majority of sales.
library(scales)

gg_bar_plot<-ggplot(bigmart_train,aes(bigmart_train$Outlet_Identifier,bigmart_train$Item_Outlet_Sales))+geom_bar(stat='identity',colour='green')+scale_y_continuous(labels=comma)+labs(x="Outlet Identifier",y="Item Outlet Sales")

gg_bar_plot
##Conclusion:- Outlet Identifier "OUT027" has contributed to the majority of Sales 

#C. Plot a Barplot using ggplot for Item_Type vs Item_Outlet_Sales also draw conclusion which items are sold more.

gg_bar_plot_1<-ggplot(bigmart_train,aes(bigmart_train$Item_Type,bigmart_train$Item_Outlet_Sales))+geom_bar(stat='identity',colour='blue')+scale_y_continuous(labels=comma)+labs(x='Item Type',y='Item Outlet Sales')+theme(axis.text.x=element_text(angle=45,hjust=1))

gg_bar_plot_1
##Conclusion:- "Fruits and Vegetables" Item Type are sold more.

#D. Plot a Boxplot using ggplot for Item_Type vs Item_MRP also draw conclusion which item is costlier
gg_boxplot<-ggplot(bigmart_train,aes(bigmart_train$Item_Type,bigmart_train$Item_MRP))+geom_boxplot()+theme(axis.text.x=element_text(angle=45,hjust=1))+labs(x='Item Type',y='Item MRP')

gg_boxplot
##Conclusion:- "Dairy" Item Type is costlier than the rest other Item Types.

#7. Manipulating Dataset to make it consistent
#A. Add Item_Outlet_Sales Column to test dataset which is'nt available & assign integer 1. Also Combine Both Train + Test Datasets.

bigmart_test$Item_Outlet_Sales<-1
bigmart_sales<-rbind(bigmart_train,bigmart_test)
dim(bigmart_sales)
str(bigmart_sales)

#B. Impute missing value in Item_Weight using median because it is highly robust to Outliers.

bigmart_sales$Item_Weight[is.na(bigmart_sales$Item_Weight)]<-median(bigmart_sales$Item_Weight,na.rm=TRUE)

View(bigmart_sales)

#C. We saw item visibility has zero value also, which is practically not feasible. Impute median value where item_visibility 0.
bigmart_sales$Item_Visibility<-ifelse(bigmart_sales$Item_Visibility==0,median(bigmart_sales$Item_Visibility),bigmart_sales$Item_Visibility)

View(bigmart_sales)

#D. Rename level in Outlet_Size to since mis-matched levels in variables needs to be corrected.
bigmart_sales$Outlet_Size<-as.factor(bigmart_sales$Outlet_Size)
nlevels(bigmart_sales$Outlet_Size)

bigmart_sales$Outlet_Size
levels(bigmart_sales$Outlet_Size)[1]<-"Other"

#E. Rename levels of Item_Fat_Content since value are "LF" / "low fat", so make them consistent.
library(plyr)
library(dplyr)

bigmart_sales$Item_Fat_Content<-revalue(bigmart_sales$Item_Fat_Content,c("LF"="Low Fat","reg"="Regular","low fat"="Low Fat"))

#F. Create a new column 2013 - Year ( For Prediction ).
bigmart_sales$Year<-2013 -bigmart_sales$Outlet_Establishment_Year

#G. Drop variables not required in modelling i.e. Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year as they aren't needed for prediction.
bigmart_sales<-select(bigmart_sales,-c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year))
View(bigmart_sales)

#H. Divide data set into Train and Test.
bigmart_train1<-bigmart_sales[1:nrow(bigmart_train),]
bigmart_test1<-bigmart_sales[-(1:nrow(bigmart_train)),]

#I. Perform a Regression testing on training dataset
relation<-lm(bigmart_train1$Item_Outlet_Sales~.,data=bigmart_train1)

#J. Plot Summary and Predict sales for Testing Dataset.
y_prediction<-predict(relation,bigmart_test1)
summary(relation)
View(y_prediction)
