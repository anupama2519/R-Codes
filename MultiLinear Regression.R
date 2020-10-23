#Multiple Linear Regression :

#Question 1
#Prepare a prediction model for profit of 50_startups data.

#Read a CSV File :

#startup<-read.csv(file.choose())
startup<-read.csv("C:\\Users\\admin\\Desktop\\python program files\\Assignment 5\\50_Startups.csv")
View(startup)
summary(startup)
class(startup)


#Need to deal with categorical data column (State) :
#Use revalue function to convert the char values into number.

unique(startup$State)
#"New York"   "California" "Florida"
install.packages("plyr")
library(plyr)

startup$State<-as.integer(revalue(startup$State,c("New York"="0","California"="1","Florida"="2")))
View(startup)

#Check for Null Values :
is.na(startup)

# There are no Null values.

#Normaliy Check:

#Graphical Representation
#QQ Plot

qqnorm(startup$R.D.Spend)
qqline(startup$R.D.Spend)

qqnorm(startup$Administration)
qqline(startup$Administration)

qqnorm(startup$Marketing.Spend)
qqline(startup$Marketing.Spend)

qqnorm(startup$Profit)
qqline(startup$Profit)

#Histogram
hist(startup$R.D.Spend)
hist(startup$Administration)
hist(startup$Marketing.Spend)
hist(startup$Profit)

#Boxplot Representation
A<-boxplot(startup$R.D.Spend)
B<-boxplot(startup$Administration)
C<-boxplot(startup$Marketing.Spend)
D<-boxplot(startup$Profit)

#Outliers :
A$out     #No
B$out     #No
C$out     #No
D$out     #No

#Calculate Mean and Median :
summary(startup)

#R D Spend          Administration      Marketing.Spend       Profit
#Mean =73721.62     mean=121344.6       mean=211025.1         mean=112012.6
#Median=73051.08    median=122699.8     median=212716.2       median=107978.2

#No need of Transformation

attach(startup)

#Plot relation between each X and Y:

plot(R.D.Spend,Profit)         #Linear relation - strong
plot(Administration,Profit)    #Not following Linear Relation 
plot(Marketing.Spend,Profit)   #Linear relation - moderate
plot(State,Profit)             #Not following Linear Relation

pairs(startup)

#Correlation Between Input and Output Variables:
cor(R.D.Spend,Profit)           #0.9729005      Strong
cor(Administration,Profit)      #0.2007166
cor(Marketing.Spend,Profit)     #0.7477657      Moderate
cor(State,Profit)               #0.04847097

cor(startup)

# R D Spend ,Profit are having strong correlation and Marketing , Profit are having Moderate Correlation.


#Model1 :

startup_model1<-lm(Profit~R.D.Spend+Administration+Marketing.Spend+State,data=startup)
summary(startup_model1)

#P<0.05, Multiple R-squared:  0.9508,	Adjusted R-squared:  0.9464 

#Prediction:
predict_model1<-predict(startup_model1,interval = "predict")
View(predict_model1)


#VIF:
vif(startup_model1)


startup_model1$coefficients
startup_model1$residuals

plot(startup_model1)

#Model with  R D Spend as I/P and Profit as O/P
startup_modelRD<-lm(Profit~R.D.Spend,data=startup)
summary(startup_modelRD)
#p<0.05 and Multiple R-squared:  0.9465,	Adjusted R-squared:  0.9454 
 
#Model with  Administration as I/P and Profit as O/P
startup_modelAdm<-lm(Profit~Administration,data=startup)
summary(startup_modelAdm)
#P<0.05 andMultiple R-squared:  0.04029,	Adjusted R-squared:  0.02029  

plot(startup_modelAdm)

#Model with R D Spend , Administration together as I/P and Profit as O/P
startup_RD_Adm<-lm(Profit~Administration+R.D.Spend,data=startup)
summary(startup_RD_Adm)
#P<0.05 and Multiple R-squared:  0.9478,	Adjusted R-squared:  0.9456


#Identification of Outliers and influential variable :

library(car)

qqPlot(startup_model1)  # Outliers 46th and 50th row
influence.measures(startup_model1)
influencePlot(startup_model1)    #49th and 50th row

#Diagnostic Plot
influenceIndexPlot(startup_model1)

#Model with deleting 49th and 50th record :
#Model 2:

startup_model1_del<-lm(Profit~R.D.Spend+Administration+Marketing.Spend+State,data=startup[-c(49,50),])
summary(startup_model1_del)


#p<0.05 and Multiple R-squared:  0.9627,	Adjusted R-squared:  0.9592

#Check collinearity Problem

vif(startup_model1_del)
#VIF<10  .. No Collinearity Problem

#Added Variable Plots
avPlots(startup_model1_del,col="red")


#Prediction:
predict_model2<-predict(startup_model1_del,interval = "predict")
View(predict_model2)


#Model 3:

startup_model2_del<-lm(Profit~R.D.Spend+log(Administration)+Marketing.Spend+State,data=startup[-c(49,50),])
summary(startup_model2_del)

#Multiple R-squared:  0.9625,	Adjusted R-squared:  0.959 
#p<0.05

avPlots(startup_model2_del,col="blue")

vif(startup_model2_del)   #VIF<10

#Prediction:
predict_model3<-predict(startup_model2_del,interval = "predict")
View(predict_model3)


#QQPlot of various Models:  

qqPlot(startup_model1)
qqPlot(startup_model1_del)
qqPlot(startup_model2_del)


# Check AIC:
install.packages("MASS")
library(MASS)

stepAIC(startup_model1_del)
 

#Lower the AIC, Better is the model.



#Question 2
#Predict Price of the computer

#Read a CSV File :

#computerdata<-read.csv(file.choose())
computerdata<-read.csv("C:\\Users\\admin\\Desktop\\python program files\\Assignment 5\\Computer_Data.csv")
View(computerdata)

#First column(Index) is not required. Delete First Column -
computerdata<-computerdata[,-1]
View(computerdata)

class(computerdata)

#Need to deal with categorical Data:

#Use revalue function to convert the categorical data in Binary digit
library(plyr)

unique(computerdata$cd)     #no , yes
unique(computerdata$multi)  #no , yes
unique(computerdata$premium) #no, yes

#Column cd :
computerdata$cd<-as.integer(revalue(computerdata$cd,c("no"="0","yes"="1")))
#Column multi :
computerdata$multi<-as.integer(revalue(computerdata$multi,c("no"="0","yes"="1")))
#Column premium :
computerdata$premium<-as.integer(revalue(computerdata$premium,c("no"="0","yes"="1")))

View(computerdata)

#Preprocessing of Data:

#Check for Null Values :

unique(is.na(computerdata))

#No Null Values.

#Outliers :

A<-boxplot(computerdata$price)  #yes
B<-boxplot(computerdata$speed)  #No
C<-boxplot(computerdata$hd)     #yes
D<-boxplot(computerdata$ram)    #yes
E<-boxplot(computerdata$screen) #Only 1

#Outlier Treatment :
#Function to remove outliers :

outlierTreament<-function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)}

#Column Price :
computerdata$price<-outlierTreament(computerdata$price)
A1<-boxplot(computerdata$price)   #No

#Column hd :
computerdata$hd<-outlierTreament(computerdata$hd)
C1<-boxplot(computerdata$hd)   #only 1

#Column ram :
computerdata$ram<-outlierTreament(computerdata$ram)
D1<-boxplot(computerdata$ram)   #only 1


#Normaliy Check:

#Calculate Mean and Median :

summary(computerdata)

 #  Price            Speed           HD                RAM               SCREEN
#Median :2144   Median : 50.00   Median : 340.0   Median : 8.000   Median :14.00  
#Mean   :2220   Mean   : 52.01   Mean   : 416.6   Mean   : 8.287   Mean   :14.61 

 #   ADS           Trend
#Median :246.0  Median: 16.00
#Mean   :221.3  Mean  : 15.93

#Graphical Representation :

#Histogram:
hist(computerdata$price)
hist(computerdata$speed)
hist(computerdata$hd)
hist(computerdata$ram)
hist(computerdata$screen)

#QQ Plot:

qqnorm(computerdata$price)
qqline(computerdata$price)

qqnorm(computerdata$speed)
qqline(computerdata$speed)

qqnorm(computerdata$hd)
qqline(computerdata$hd)

qqnorm(computerdata$ram)
qqline(computerdata$ram)

qqnorm(computerdata$screen)
qqline(computerdata$screen)

#Standardisation to make the data scale free:

computerdata_standard<-as.data.frame(scale(computerdata))
View(computerdata_standard)
summary(computerdata_standard)

attach(computerdata_standard)

#Plot relationship between each X and Y:
plot(speed,price)    
plot(hd,price)
plot(ram,price)
plot(screen,price)
plot(cd,price)
plot(multi,price)
plot(premium,price)
plot(ads,price)
plot(trend,price)

pairs(computerdata)

#Correlation between Input and Output Variables:
cor(speed,price)       #0.3049     
cor(hd,price)          #0.436
cor(ram,price)         #0.6496
cor(screen,price)      #0.2874
cor(cd,price)          #0.2077
cor(multi,price)       #-0.0100
cor(premium,price)     #-0.078
cor(ads,price)         #0.056
cor(trend,price)       #-0.1936

cor(computerdata)

# No Input variable is having strong relation with Output Variable. 

#Model : each I/P variable with O/P Variable :

model_speed<-lm(price~speed,data=computerdata_standard)
summary(model_speed)
#p<0.05
#Multiple R-squared:  0.09297,	Adjusted R-squared:  0.09283  .... Very less

model_hd<-lm(price~hd,data=computerdata_standard)
summary(model_hd)
#p<0.05
#Multiple R-squared:  0.1907,	Adjusted R-squared:  0.1905 ...... Very less

model_ram<-lm(price~ram,data=computerdata_standard)
summary(model_ram)
#p<0.05
#Multiple R-squared:  0.4221,	Adjusted R-squared:  0.422 ...... Very less

model_screen<-lm(price~screen,data=computerdata_standard)
summary(model_screen)
#p<0.05
#Multiple R-squared:  0.08261,	Adjusted R-squared:  0.08246 ...... Very less

model_cd<-lm(price~cd,data=computerdata_standard)
summary(model_cd)
#p<0.05
#Multiple R-squared:  0.04315,	Adjusted R-squared:  0.043  ...... Very less

model_multi<-lm(price~multi,data=computerdata_standard)
summary(model_multi)
#p<0.05
#Multiple R-squared:  0.0001011,Adjusted R-squared:  -5.875e-05 .... Very less

model_premium<-lm(price~premium,data=computerdata_standard)
summary(model_premium)
#p<0.05
#Multiple R-squared:  0.006093,	Adjusted R-squared:  0.005934...... Very less

model_ads<-lm(price~ads,data=computerdata_standard)
summary(model_ads)
#p<0.05
#Multiple R-squared:  0.003158,	Adjusted R-squared:  0.002999  ...... Very less

model_trend<-lm(price~trend,data=computerdata_standard)
summary(model_trend)
#p<0.05
#Multiple R-squared:  0.03751,	Adjusted R-squared:  0.03736  ...... Very less

#Model Building:
#Model1 :

model1_computerdata<-lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data=computerdata_standard)
summary(model1_computerdata)
#p<0.05
#Multiple R-squared:  0.7845,	Adjusted R-squared:  0.7842  .. Moderate


#Prediction:
prediction_computerdata<-predict(model1_computerdata,interval = "predict")
View(prediction_computerdata)

#Check for collinearity : VIF
library(car)
vif(model1_computerdata)

#vif<10   .. No Collinearity Problem

model1_computerdata$coefficients
model1_computerdata$residuals

plot(model1_computerdata)

#Identify influential Variable:
qqPlot(model1_computerdata)   #208 310
influence.measures(model1_computerdata)
influencePlot(model1_computerdata)    # 80  208  310  1689  1785 4410 

#Diagnostic Plot:
influenceIndexPlot(model1_computerdata)


#Build other models for better R Squared value :
#Model2 with deleting 208 and 310 row :

model2_computerdata<-lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data=computerdata_standard[-c(208,310),])
summary(model2_computerdata)
#p<0.05
#Multiple R-squared:  0.7858,	Adjusted R-squared:  0.7855 

model2_computerdata$coefficients
model2_computerdata$residuals


#Check for collinearity : VIF

vif(model2_computerdata)

#vif<10   .. No Collinearity Problem

#Prediction:
prediction2_computerdata<-predict(model2_computerdata,interval = "predict")
View(prediction2_computerdata)

#Model3 with deleting 80,208,310 and 4410 rows:
model3_computerdata<-lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data=computerdata_standard[-c(80,208,310,4410),])
summary(model3_computerdata)
#p<0.05
#Multiple R-squared:  0.7868,	Adjusted R-squared:  0.7865

model3_computerdata$coefficients
model3_computerdata$residuals


#Check for collinearity : VIF

vif(model3_computerdata)

#vif<10   .. No Collinearity Problem

#Prediction:
prediction3_computerdata<-predict(model3_computerdata,interval = "predict")
View(prediction3_computerdata)

#Model4 without cd,multi and premium variables:
model4_computerdata<-lm(price~speed+hd+ram+screen+ads+trend,data=computerdata_standard[-c(80,208,310,4410),])
summary(model4_computerdata)
#p<0.05
#Multiple R-squared:  0.7189,	Adjusted R-squared:  0.7186

model4_computerdata$coefficients
model4_computerdata$residuals


#Check for collinearity : VIF

vif(model4_computerdata)

#vif<10   .. No Collinearity Problem

#Prediction:
prediction4_computerdata<-predict(model4_computerdata,interval = "predict")
View(prediction4_computerdata)

#avplots :
avPlots(model1_computerdata)
avPlots(model2_computerdata)
avPlots(model3_computerdata)
avPlots(model4_computerdata)

#Check AIC :
library(MASS)

stepAIC(model1_computerdata)

#Result : R Squared value of all the models are approx same.



#Q3.
#Consider only the below columns and prepare a prediction model for predicting Price.

#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

#Read CSV File:

#corolla<-read.csv(file.choose())
corolla_full<-read.csv("C:\\Users\\admin\\Desktop\\python program files\\Assignment 5\\ToyotaCorolla.csv")
View(corolla_full)
ncol(corolla_full)    #38
colnames(corolla_full)

#Extract the required columns (Given in Problem):
corolla<-corolla_full[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(corolla)
class(corolla)

#Preprocessing of Data:

#Check for Null Values:

is.na(corolla)  #No Null Values

#Check for Outliers:
#Function to remove the outliers based on Interquartile Range :

outlierTreament<-function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)}

#Column Price:

A<-boxplot(corolla$Price,horizontal = T)
A$out                                 #yes

corolla$Price<-outlierTreament(corolla$Price)
A1<-boxplot(corolla$Price,horizontal = T)   #one 

#Age_08_04 Column

B<-boxplot(corolla$Age_08_04,horizontal = T)  #yes
corolla$Age_08_04<-outlierTreament(corolla$Age_08_04)
B1<-boxplot(corolla$Age_08_04,horizontal = T)
B1$out                                #No
  

#KM Column
C<-boxplot(corolla$KM,horizontal = T)  #yes

corolla$KM<-outlierTreament(corolla$KM)
C1<-boxplot(corolla$KM,horizontal = T)
C1$out                              #No


# HP Column:
D<-boxplot(corolla$HP,horizontal = T)
D$out                              #192

#No Need to treat as it is having only one outlier


#CC Column :

E<-boxplot(corolla$cc,horizontal = T)
E$out                     #yes
  
corolla$cc<-outlierTreament(corolla$cc)
E1<-boxplot(corolla$cc,horizontal = T)
E1$out                          #2000 

#Doors Column:

F<-boxplot(corolla$Doors,horizontal = T)
F$out                        #No


#Quarterly_tax Column:

H<-boxplot(corolla$Quarterly_Tax,horizontal = T)
H$out                      #yes

corolla$Quarterly_Tax<-outlierTreament(corolla$Quarterly_Tax)
H1<-boxplot(corolla$Quarterly_Tax,horizontal = T)
H1$out                      #only two outliers

#Weight Column:

I<-boxplot(corolla$Weight,horizontal = T)
I$out       #yes

corolla$Weight<-outlierTreament(corolla$Weight)

I1<-boxplot(corolla$Weight,horizontal = T)
I1$out                             #No


#Calculate Mean and Median:

summary(corolla)

#Price            Age              KM               HP              cc      
#Median : 9900   Median :61.00   Median : 63390   Median :110.0   Median :1600  
#Mean   :10635   Mean   :56.03   Mean   : 67106   Mean   :100.9   Mean   :1567  
 
#  Doors            Gears      Quarterly_Tax        Weight    
#Median :4.000   Median :5       Median : 85.00   Median :1070  
#Mean   :4.033   Mean   :5.026   Mean   : 86.64   Mean   :1068  


#Graphical Representation:

#QQ Plot:

qqnorm(corolla$Price)
qqline(corolla$Price)

qqnorm(corolla$Age)
qqline(corolla$Age)

qqnorm(corolla$KM)
qqline(corolla$KM)

qqnorm(corolla$HP)
qqline(corolla$HP)

qqnorm(corolla$cc)
qqline(corolla$cc)

qqnorm(corolla$Doors)
qqline(corolla$Doors)

qqnorm(corolla$Gears)
qqline(corolla$Gears)

qqnorm(corolla$Quarterly_Tax)
qqline(corolla$Quarterly_Tax)

qqnorm(corolla$Weight)
qqline(corolla$Weight)

#Histogram:

hist(corolla$Price)
hist(corolla$Age)
hist(corolla$KM)
hist(corolla$HP)
hist(corolla$cc)
hist(corolla$Doors)
hist(corolla$Gears)
hist(corolla$Quarterly_Tax)
hist(corolla$Weight)

#Data is not following Normal Distribution

#standardisation :

corolla_standard<-as.data.frame(scale(corolla))
summary(corolla_standard)

attach(corolla_standard)

colnames(corolla_standard)
#"Price" "Age_08_04" "KM" "HP"  "cc"  "Doors" "Gears" "Quarterly_Tax"
#"Weight"

#Plot relationship between each X and Y:
plot(Age_08_04,Price)   #High Price for New car
plot(KM,Price)          
plot(HP,Price)
plot(cc,Price)
plot(Doors,Price)
#plot(Gears,Price)
plot(Quarterly_Tax,Price)
plot(Weight,Price)

#pairs(corolla_standard)

#Correlation between Input and Output Variables:

cor(Age_08_04,Price)   #-0.8917582  ... strong but negative
cor(KM,Price)          #-0.6070784  ... Moderate(Negative)  
cor(HP,Price)          #0.2913787   ... very low
cor(cc,Price)          #0.1356133   ... Very low
cor(Doors,Price)       #0.1819627   ... Very low    
cor(Gears,Price)       # NA
cor(Quarterly_Tax,Price) # 0.1343582  ... very low
cor(Weight,Price)       #0.5238864    ...  low

#Only Age and KM have good correlation with Price

#Model : each I/P variable with O/P Variable :

model_age<-lm(Price~Age_08_04,data=corolla_standard)
summary(model_age) 
#p<0.05
#Multiple R-squared:  0.7952,	Adjusted R-squared:  0.7951 ... Moderate

model_km<-lm(Price~KM,data = corolla_standard)
summary(model_km)
#p<0.05
#Multiple R-squared:  0.3685,	Adjusted R-squared:  0.3681 

model_hp<-lm(Price~HP,data = corolla_standard)
summary(model_hp)
#p<0.05
#Multiple R-squared:  0.0849,	Adjusted R-squared:  0.08426

model_cc<-lm(Price~cc,data = corolla_standard)
summary(model_cc)
#p<0.05
#Multiple R-squared:  0.01839,	Adjusted R-squared:  0.01771

model_door<-lm(Price~Doors,data = corolla_standard)
summary(model_door)
#p<0.05
#Multiple R-squared:  0.03311,	Adjusted R-squared:  0.03244

model_gear<-lm(Price~Gears,data = corolla_standard)
summary(model_gear)
#p=0.05
#Multiple R-squared:  0.003104,	Adjusted R-squared:  0.002408
#Not significant

model_qt<-lm(Price~Quarterly_Tax,data = corolla_standard)
summary(model_qt)
#p<0.05
#Multiple R-squared:  0.01805,	Adjusted R-squared:  0.01737

model_wt<-lm(Price~Weight,data = corolla_standard)
summary(model_wt)
#p<0.05
#Multiple R-squared:  0.2745,	Adjusted R-squared:  0.274

#Model Building:

#Model1 :
#With only Age,KM and HP variable :

model_corolla1<-lm(Price~Age_08_04+KM+HP,data = corolla_standard)
summary(model_corolla1)
#p<0.05
#Multiple R-squared:  0.8317,	Adjusted R-squared:  0.8314 

#Prediction:
prediction_corolla1<-predict(model_corolla1,interval = "predict")
View(prediction_corolla1)

#Check for collinearity : VIF
library(car)
vif(model_corolla1)


#Model2 :
#With all I/P Variable :

model_corolla2<-lm(Price~.,data = corolla_standard)
summary(model_corolla2)

#p<0.05
#Multiple R-squared:  0.8659,	Adjusted R-squared:  0.8651  ... strong

#Prediction:
prediction_corolla2<-predict(model_corolla2,interval = "predict")
View(prediction_corolla2)

#Check for collinearity : VIF
library(car)
vif(model_corolla2)

#vif<10   .. No Collinearity Problem

plot(model_corolla2)

#Identify influential Variable:
qqPlot(model_corolla1)   #524 602
influence.measures(model_corolla1)
influencePlot(model_corolla1)    # 524  602   957   992 

#Diagnostic Plot:
influenceIndexPlot(model_corolla1)


#Build other models for better R Squared value :
#Model3 with deleting 524  and 602 row :

model_corolla3<-lm(Price~.,data = corolla_standard[-c(524,602),])
summary(model_corolla3)
#p<0.05
#Multiple R-squared:  0.8698,	Adjusted R-squared:  0.869


#Prediction:
prediction_corolla3<-predict(model_corolla3,interval = "predict")
View(prediction_corolla3)

#Check for collinearity : VIF
#library(car)
vif(model_corolla3)

#vif<10   .. No Collinearity Problem

plot(model_corolla3)

#avplots :
avPlots(model_corolla1)
avPlots(model_corolla2)
avPlots(model_corolla3)


#Check AIC :
library(MASS)

stepAIC(model_corolla3)

#Result : Lower the AIC, better the model.





