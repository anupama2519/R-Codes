#Q1.
#Classify whether application accepted or not using Logistic regression

#Read a CSV File :

#credit_card<-read.csv(file.choose())
credit_card<-read.csv("C:\\Users\\admin\\Desktop\\python program files\\Assignment 6\\creditcard.csv")
View(credit_card)
ncol(credit_card)    #13

colnames(credit_card)

#"card"        "reports"     "age"         "income"      "share"       "expenditure" "owner"      
#"selfemp"     "dependents"  "months"      "majorcards"  "active"     


#First column is not required. so, delete column 1 :

credit_card<-credit_card[,2:13]
View(credit_card)

str(credit_card)

table(credit_card$card)

#no  yes 
#296 1023


#Need to deal with categorical data (owner and selfemp) :
#Convert categorical data into Binary

#Use revalue function to convert data into binary digits.

library(plyr)   #required for revalue function

#Converting card column into Binary digit
unique(credit_card$card)     #yes , no

credit_card$card<-as.integer(revalue(credit_card$card,c("yes"="1","no"="0")))


#Converting ownercolumn to binary digits
unique(credit_card$owner)     #yes , no

credit_card$owner<-as.integer(revalue(credit_card$owner,c("yes"="1","no"="0")))

#Converting Selfemp column into Binary digit
unique(credit_card$selfemp)   #no , yes

credit_card$selfemp<-as.integer(revalue(credit_card$selfemp,c("no"="0","yes"="1")))

View(credit_card)

dim(credit_card)  #1319     12

#Preprocessing of data :

#Checking for Null Values :

unique(is.na(credit_card))    #False

#No Null Values

# Graphichal Representation :

#Histogram
#Continuous Variables

hist(credit_card$age)
hist(credit_card$income)
hist(credit_card$share)
hist(credit_card$expenditure)


#Boxplot
#Continuous Variables

A<-boxplot(credit_card$age,horizontal = T)
B<-boxplot(credit_card$income,horizontal = T)
C<-boxplot(credit_card$share,horizontal = T)
D<-boxplot(credit_card$expenditure,horizontal = T)

#Outliers:
A$out
#Yes

B$out
#Yes

C$out
#yes

D$out
#Yes

#Outlier Treatment :

#Function to remove the outliers based on Interquartile Range :

outlierTreament<-function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)}

#Column age :

credit_card$age<-outlierTreament(credit_card$age)
A1<-boxplot(credit_card$age)   #No outlier

#Column income :

credit_card$income<-outlierTreament(credit_card$income)
B1<-boxplot(credit_card$income)   # Only one outlier

#Column share :

credit_card$share<-outlierTreament(credit_card$share)
C1<-boxplot(credit_card$share)    #only one outlier

#Column expenditure  :

credit_card$expenditure<-outlierTreament(credit_card$expenditure)
D1<-boxplot(credit_card$expenditure)   #Only one outliers


#QQ Plot :
qqnorm(credit_card$age)
qqline(credit_card$age)

qqnorm(credit_card$income)
qqline(credit_card$income)

qqnorm(credit_card$share)
qqline(credit_card$share)

qqnorm(credit_card$expenditure)
qqline(credit_card$expenditure)

#mean and median :
summary(credit_card)

#    reports       age             income          share
# Median:0.00     Median:31.25    Median:2.90    Median:0.0388
# Mean  :0.4564   Mean  :33.10    Mean  :3.284   Mean  :0.0622

#   expenditure
#  Median:101.298
#  Mean  :164.667 
            
#  dependents         months         majorcards         active      
# Median :1.0000   Median : 30.00   Median :1.0000   Median : 6.000  
# Mean   :0.9939   Mean   : 55.27   Mean   :0.8173   Mean   : 6.997  

attach(credit_card)


#Model :


model1_creditcard<- glm(card~.,data = credit_card,family = "binomial")
summary(model1_creditcard)
#p>0.05

plot(model1_creditcard)

#calculate the probability :

probability<-predict(model1_creditcard,type = c("response"),credit_card)

#Confusion Matrix:
confusion_credit<-table(probability>0.5,credit_card$card)
#        0    1
#FALSE  131    6
#TRUE   165 1017

#Check the model accuracy :
acc<-sum(diag(confusion_credit)/sum(confusion_credit))
acc    #0.8703563


#Predict the card value based on Probabilities:
predict_card<-NULL

for (i in 1:1319){
  predict_card[i]=ifelse(probability[i]>=0.5,1,0)
}

#Add probability and predict_card column in credit_card data set:
credit_card["prob"]<-probability
credit_card["prediction"]<-predict_card

View(credit_card)
View(credit_card[,c(1,14)])

acc1<-table(credit_card$card,credit_card$prediction)
#     0    1
#0  131  165
#1    6 1017

sum(diag(acc1)/sum(acc1))   #0.870  .... Strong

#ROC curve :
library(ROCR)
pred_rocr<-prediction(probability,credit_card$card)   #prediction
perf_rocr<-performance(pred_rocr,'tpr','fpr')         #performance

plot(perf_rocr)     
#More area under the ROC curve, better the model



#Q2.
#Predict whether the client has subscribed a term deposit or not 

#Read csv data :
bank<-read.csv(file.choose(),sep=";")
View(bank)
ncol(bank)   #17
colnames(bank)
# "age"       "job"       "marital"   "education" "default"   "balance"   "housing"  
#"loan"      "contact"   "day"       "month"     "duration"  "campaign"  "pdays"    
# "previous"  "poutcome"  "y"     

str(bank)
table(bank$y)
#no   yes 
#39922  5289


#Need to deal with categorical data  :
#Convert categorical data into Binary

#Use revalue function to convert data into binary digits.

library(plyr)   #required for revalue function

library(fastDummies)
bank_new<-dummy_cols(bank[,1:16])
ncol(bank_new)   #60
View(bank_new)

bank_new["Output"]<-bank$y
ncol(bank_new)    #61

unique(bank_new$Output)   #no, yes
bank_new$Output<-as.integer(revalue(bank_new$Output,c("no"="0","yes"="1")))

#Delete columns 2,3,4,5,7,8,9,11,16 - categorical columns

bank_new<-bank_new[,-c(2,3,4,5,7,8,9,11,16)]
View(bank_new)
colnames(bank_new)

#Preprocessing of data :

#Check for Null Values:

unique(is.na(bank_new))

#there are no NULL values.

summary(bank_new) 

#Model Building :
  
attach(bank_new)

model_bank<-glm(Output~.,data = bank_new,family = "binomial")  
summary(model_bank)

#Calculate the probability
prob_bank<-predict(model_bank,type=c("response"),bank_new)

#Confusion Matrix :
confusion_bank<-table(prob_bank>0.5,bank_new$Output)

#         0     1
#FALSE 38940  3456
#TRUE    982  1833

#Accuracy
acc_bank<-sum(diag(confusion_bank))/sum(confusion_bank)
acc_bank       #0.9018 ...... Strong

#Predict the Output based on Probability:
pred_bank<-NULL

for (i in 1:45211){
  pred_bank[i]=ifelse(prob_bank>=0.5,1,0)
} 

#Add prob_bank and Pred_bank column in bank data set :
bank_new["probability"]<-prob_bank
bank_new["Predicted Value"]<-pred_bank

View(bank_new)
View(bank_new[,52:54])

conf_pred<-table(bank_new$Output,bank_new$`Predicted Value`)
accuracy_model<-sum(diag(conf_pred))/sum(conf_pred)
accuracy_model    #0.88......... strong

#ROC Curve:

library(ROCR)

pred_rocr<-prediction(prob_bank,bank_new$Output)
perf_rocr<-performance(pred_rocr,"tpr","fpr")
plot(perf_rocr)

#This curve covers max part of area

#More area under the RIC Curve, better the model.