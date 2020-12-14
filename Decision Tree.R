# Decision Tree
#Q1.
#A cloth manufacturing company is interested to know about the segment or 
#attributes causes high sale. 

#Read csv file:
company<-read.csv(file.choose())
View(company)
summary(company)     #Combination of continuous and categorical variables.
dim(company)    #400    11

#Preprocessing of data:

#Check for null Values:
unique(is.na(company))   #False

#Check for outliers:
boxplot(company$CompPrice)            #Only two outliers
boxplot(company$Income)               #No
boxplot(company$Advertising)          #No
boxplot(company$Population)           #No
boxplot(company$Price,horizontal = T) #yes
boxplot(company$Age)                  #No
boxplot(company$Education)            #No

#Check for the sales graph :

hist(company$Sales,col=c("red","blue","green"))
# As per the histogram, distribution is approx normal.
#Sales freq is high between 6 to 8.


#Since Output variable is sales(Continuous), we need to convert it into categorical.
#Assume that if sales>8, High and sales<8, Low


output=ifelse(company$Sales>8,"yes","no")
View(output)
company_new=cbind(company,as.factor(output))  #we want output variable to be factor level
dim(company_new)   #400  12
View(company_new)

#We dont require column 1(sales), so delete the column 1:
company_new<-company_new[,-1]
View(company_new)

#Change the last column name to Sales:
colnames(company_new)[11]<-"Sales"


#attach(company_new)

#Split the data into Test and Train :

#Need to import caTools package:
library(caTools)
set.seed(50)   # used to get the same split each time it run.

#we will use sample.split function to split the data in train and test :

company_sample<-sample.split(company_new$CompPrice,SplitRatio = 0.70)
company_train<-subset(company_new,company_sample==TRUE)
company_test<-subset(company_new,company_sample==FALSE)
dim(company_train)   #284  11
View(company_train)
dim(company_test)    #116 11

# Total no of records : train + test = 400.
#It shows split is done properly

#Model Building on Train data :


# **********************1.  using D tree :***********************************

#Need to import tree package to build decision tree:
library(tree)
#install.packages("C50")
#install.packages("caret")
#library(C50)
#library(caret)
attach(company_train)

tree_comp<-tree(company_train$Sales~.,data = company_train)
summary(tree_comp)

#Variables actually used in tree construction:
#"Price"       "CompPrice"   "Age"         "Advertising" "Income"      "Population" 
#Number of terminal nodes:  21 
#Residual mean deviance:  0.5841 = 153.6 / 263 
#Misclassification error rate: 0.1303 = 37 / 284 

#Graphical Representation:

plot(tree_comp)
text(tree_comp)

#From the decision tree we can figure out that, If price is less than 89.5, then there are 
#more chances of customer to buy.
#If price is greater than 89.5 then with Local Advertising Budget>13.5 and 
#Competitor Price>121.5, chances are better for customer to buy.

#Prediction on Training Data Set:

pred_company_train<-as.data.frame(predict(tree_comp,company_train))
View(pred_company_train)
dim(pred_company_train)

pred_company_train["final"]<-NULL
for (i in 1:nrow(pred_company_train)){
  pred_company_train[i,"final"]<-ifelse(pred_company_train[i,"no"]>0.8,"no","yes")
}

#Accuracy :

#By calculating Mean:
mean(pred_company_train$final==company_train$Sales)   #77.46%


#By cross table

table_train<-table(company_train$Sales,pred_company_train$final)
accuracy<-sum(diag(table_train))/sum(table_train)    #77.46%

#Result : Accuracy is moderate.


#Prediction on Test Data Set:

pred_company<-as.data.frame(predict(tree_comp,company_test))
View(pred_company)
dim(pred_company)

pred_company["final"]<-NULL
for (i in 1:nrow(pred_company)){
  pred_company[i,"final"]<-ifelse(pred_company[i,"no"]>0.8,"no","yes")
}

#Accuracy :

#By calculating Mean:
mean(pred_company$final==company_test$Sales)   #0.620


#By cross table

table<-table(company_test$Sales,pred_company$final)
accuracy<-sum(diag(table))/sum(table)    #0.620

#Result : Accuracy is moderate.

# **********************2.  using c5.0 package :***********************************
library(caret)
library(C50)

comp_c50<-C5.0(company_train$Sales~.,data = company_train)
summary(comp_c50)

#Graphical Representation :

plot(comp_c50)

#From the decision tree we can figure out that, If shelveLoc is Good and Price is less, then
#there are 40% chances of customer to buy.
#If ShelveLoc is Bad or Medium, Price>88,Advertising>13 and Competitor Price>122
#then there are 80% chances of customer to buy

#Prediction on Training Data set:

pred_c50_train<-predict(comp_c50,company_train)
View(pred_c50_train)

#Accuracy of Training Data Set :

#By mean method:
mean(pred_c50_train==company_train$Sales)
#Accuracy is 91.54%

#By cross table:

table_c50_train<-table(pred_c50_train,company_train$Sales)
Accuracy<-sum(diag(table_c50_train))/sum(table_c50_train)  
Accuracy             #91.54%


#Prediction on Test Data Set :

pred_c50<-predict(comp_c50,company_test)
View(pred_c50)

#Accuracy :

#By mean method:
mean(pred_c50==company_test$Sales)
#Accuracy is 83.62%

#By cross table:

table_c50<-table(pred_c50,company_test$Sales)
Accuracy<-sum(diag(table_c50))/sum(table_c50)  
Accuracy             #83.62

#Result :  Using C.50, Decision Tree is giving better Accuracy.
#So, final model will be comp_c50.

#Q2.
#Use decision trees to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"

#Read csv file:
fraud<-read.csv(file.choose())
View(fraud)
summary(fraud)     #Combination of continuous and categorical variables.
dim(fraud)    #600    6

#Preprocessing of data:

#Check for null Values:
unique(is.na(fraud))   #False

#Check for outliers:
boxplot(fraud$Taxable.Income,horizontal=T)            #No
boxplot(fraud$City.Population)                        #No
boxplot(fraud$Work.Experience)                        #No

#Check for the Taxable Income graph :

hist(fraud$Taxable.Income,col=c("red","blue"))

#Since Output variable is Taxable Income(Continuous), we need to convert it into categorical.
#As mentioned in problem, If taxable_income <= 30000 , "Risky" and taxable_income > 30000, "Good" 

Taxable_Income=ifelse(fraud$Taxable.Income<=30000,"Risky","Good")
View(Taxable_Income)
table(Taxable_Income)

#Good Risky 
#476   124
#Out of 600, only 124 cases are risky.

fraud_new=cbind(fraud,as.factor(Taxable_Income))  #we want output variable to be factor level
dim(fraud_new)   #600  7
View(fraud_new)
str(fraud_new)

#Undergrad,Marital Status and Urban are Character Variables.
#We have to change these variables into factor.
fraud_new[,"Undergrad"]<-as.factor(fraud_new[,"Undergrad"])
fraud_new[,"Marital.Status"]<-as.factor(fraud_new[,"Marital.Status"])
fraud_new[,"Urban"]<-as.factor(fraud_new[,"Urban"])

View(fraud_new)
str(fraud_new)  #Changed to factor type

#We dont require column 3(Taxable Income), so delete the column 3:
fraud_new<-fraud_new[,-3]
View(fraud_new)

#Change the last column name to Tax_Income:
colnames(fraud_new)[6]<-"Tax_Income"

#Split the data into Test and Train :

#Need to import caTools package:
library(caTools)
set.seed(50)   # used to get the same split each time it runs.

#we will use sample.split function to split the data in train and test :

fraud_sample<-sample.split(fraud_new$Tax_Income,SplitRatio = 0.70)
fraud_train<-subset(fraud_new,fraud_sample==TRUE)
fraud_test<-subset(fraud_new,fraud_sample==FALSE)
dim(fraud_train)   #420  6
View(fraud_train)
dim(fraud_test)    #180  6
View(fraud_test)

# Total no of records : train + test = 600.
#It shows split is done properly

#Model Building on Train data :


# **********************1.  using D tree :***********************************

#Need to import tree package to build decision tree:
library(tree)

attach(fraud_train)
#install.packages("partykit")
#library(partykit)
tree_fraud<-tree(fraud_train$Tax_Income~.,data = fraud_train)
summary(tree_fraud)

#Classification tree:
  
#Variables actually used in tree construction:
#  character(0)
#Number of terminal nodes:  1 
#Residual mean deviance:  1.023 = 428.5 / 419 
#Misclassification error rate: 0.2071 = 87 / 420

#From the summary, we can figure out that there  is only one node.
#So, we cannot plot the tree with one  node.

#Prediction on Train Data Set:

pred_fraud_train<-as.data.frame(predict(tree_fraud,fraud_train))
View(pred_fraud_train)
dim(pred_fraud_train)     #420   2
colnames(pred_fraud_train)   #"Good"  "Risky"

pred_fraud_train["final"]<-NULL
for (i in 1:nrow(pred_fraud_train)){
  pred_fraud_train[i,"final"]<-ifelse(pred_fraud_train[i,"Good"]>0.3,"Good","Risky")
}

#Accuracy :

#By calculating Mean:
mean(pred_fraud_train$final==fraud_train$Tax_Income)   #0.792


#By cross table

table_train<-table(fraud_train$Tax_Income,pred_fraud_train$final)
accuracy<-sum(diag(table_train))/sum(table_train)   
accuracy                #0.7928


#Prediction on Test Data Set:

pred_fraud<-as.data.frame(predict(tree_fraud,fraud_test))
View(pred_fraud)
dim(pred_fraud)     #180   2
colnames(pred_fraud)   #"Good"  "Risky"

pred_fraud["final"]<-NULL
for (i in 1:nrow(pred_fraud)){
  pred_fraud[i,"final"]<-ifelse(pred_fraud[i,"Good"]>0.3,"Good","Risky")
}

#Accuracy :

#By calculating Mean:
mean(pred_fraud$final==fraud_test$Tax_Income)   #0.794


#By cross table

table<-table(fraud_test$Tax_Income,pred_fraud$final)
accuracy<-sum(diag(table))/sum(table)   
accuracy                #0.794

#Result : We got same accuracy from mean and crosstable.Accuracy is moderate.


# **********************2.  using c5.0 package :***********************************
library(caret)
library(C50)

fraud_c50<-C5.0(Tax_Income~.,data = fraud_train)
summary(fraud_c50)

#Evaluation on training data (420 cases):
  
#  Decision Tree   
#----------------  
#  Size      Errors  
#1   87(20.7%)   <<
#  (a)   (b)    <-classified as

#  333          (a): class Good
#   87          (b): class Risky


#Graphical Representation :

plot(fraud_c50)

#from c5.0 package, we got the same result as ctree function. Only One node.
#20% are considered as Risky and rest 80% are considered as Good.

#Prediction on Train Data Set :

pred_fraudtrain_c50<-predict(fraud_c50,fraud_train)
View(pred_fraudtrain_c50)

#Accuracy :

#By mean method:
mean(pred_fraudtrain_c50==fraud_train$Tax_Income)
#Accuracy is 0.792

#By cross table:

table_fraudtrain_c50<-table(pred_fraudtrain_c50,fraud_train$Tax_Income)
Accuracy<-sum(diag(table_fraudtrain_c50))/sum(table_fraudtrain_c50)  
Accuracy             #0.792

#Prediction on Test Data Set :

pred_fraud_c50<-predict(fraud_c50,fraud_test)
View(pred_fraud_c50)

#Accuracy :

#By mean method:
mean(pred_fraud_c50==fraud_test$Tax_Income)
#Accuracy is 0.794

#By cross table:

table_fraud_c50<-table(pred_fraud_c50,fraud_test$Tax_Income)
Accuracy<-sum(diag(table_fraud_c50))/sum(table_fraud_c50)  
Accuracy             #0.794

#Result :  Both the methods are giving same accuracy = 0.794.
          #So we can consider any method for model building. 
#But if we want to visualize the model, then C5.0 model is best fit model.



#Q3.
#Build a decision tree for the 'iris' data with function 'ctree()' in package "party".

#Load iris data set:
iris<-datasets::iris
View(iris)
summary(iris)     #Output variable is categorical (species).
#150 observations
#Three species: setosa, versicolor, virginica
#Variables: Sepal length, Sepal width, Petal length, Petal width

dim(iris)    #150    5

#Preprocessing of data:

#Check for null Values:
unique(is.na(iris))   #False

#Check for outliers:
boxplot(iris$Sepal.Length)                         #No
boxplot(iris$Sepal.Width,horizontal = T)           #yes
boxplot(iris$Petal.Length)                         #No
boxplot(iris$Petal.Width)                          #No
# Can keep the outliers.

pairs(iris)

#From the plot, we can say that petal length and petal width are highly correlated.

#Split the data into Test and Train :
#We can split the data by many ways. 
#1. By dividing the no of rows in 70 by 30 ratio 
#2. using sample split funtion. 
#Here we are going to use sample.split function to split the data.

#Need to import caTools package:
library(caTools)
set.seed(50)   # used to get the same split each time it run.

#we will use sample.split function to split the data in train and test :

iris_sample<-sample.split(iris$Sepal.Length,SplitRatio = 0.70)
iris_train<-subset(iris,iris_sample==TRUE)
iris_test<-subset(iris,iris_sample==FALSE)
dim(iris_train)   #108  5
#View(iris_train)
dim(iris_test)    #42   5

# Total no of records : train + test = 150.
#It shows split is done properly


# **********************  using ctree function(as mentioned in problem) :***********************************
install.packages("partykit")
library(partykit)


iris_ctree<-ctree(iris_train$Species~.,data = iris_train)
summary(iris_ctree)

#Length     Class   Mode
# 7      constparty list
# 1      constparty list
# 5      constparty list
# 3      constparty list
# 1      constparty list
# 1      constparty list
# 1      constparty list


#Graphical Representation :

plot(iris_ctree)

#From the decision tree we can figure out the few things :
#1. If petal.length is less than equal to 1.9, then more than 90 % chances are there that species is Setosa.
#2. If petal.length is greater than 1.9 and petal.width is also greater than 1.6, then more 
#   than 90% chances are there that Species is Virginica and only 1% chances are for Versicolor.
#3. If petal.length is greater than 1.9 and petal.width is less than equal to 4.6, then 
#   more than 90% chances are there for Versicolor Species.
 
#Prediction on Training Data set:

pred_iris_train<-predict(iris_ctree,iris_train)
View(pred_iris_train)

#Accuracy of Training Data Set :

#By mean method:
mean(pred_iris_train==iris_train$Species)
#Accuracy is 96.29%

#By confusion Matrix:
library(caret)
confusionMatrix(iris_train$Species,pred_iris_train)
#Prediction   setosa versicolor virginica
#setosa         34          0         0
#versicolor      0         35         1
#virginica       0          3        35

#Accuracy : 0.963 

#Prediction on Test Data Set :

pred_iris_test<-predict(iris_ctree,iris_test)
View(pred_iris_test)

#Accuracy :

#By mean method:
mean(pred_iris_test==iris_test$Species)
#Accuracy is 95.23%

#By Confusion Matrix:
confusionMatrix(iris_test$Species,pred_iris_test)
#Prediction   setosa versicolor virginica
#setosa         16          0         0
#versicolor      0         13         1
#virginica       0          1        11

#Accuracy : 0.9524  

#Result :  Using ctree, training accuracy is 96.29% and test accuracy is 95.24%.
# This can be considered as best fit model.     
