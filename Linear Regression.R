#Linear Regression :

#Question 1
#Read a csv file :

cal_cons<-read.csv("C:\\Users\\admin\\Desktop\\python program files\\Assignment 3\\calories_consumed.csv")
#cal_cons <-read.csv(file.choose())    #choose calories_consumed
View(cal_cons)

#Need to import package lattice
install.packages("lattice")
library(lattice)

#calculate Mean and Median
mean(cal_cons$Weight.gained..grams.)  #357.7143
mean(cal_cons$Calories.Consumed)  #2340.714
median(cal_cons$Weight.gained..grams.)  #200
median(cal_cons$Calories.Consumed)  #2250
#mean is not equal to median

#Graphical Representation :
# To chech for Normal Distribution :

#QQ Plot
qqnorm(cal_cons$Weight.gained..grams.)
qqline(cal_cons$Weight.gained..grams.)
#Not follows Normal Distribution

qqnorm(cal_cons$Calories.Consumed)
qqline(cal_cons$Calories.Consumed)
#Not follows Normal Distribution

#Histogram
hist(cal_cons$Weight.gained..grams.)
hist(cal_cons$Calories.Consumed)

#Check for outliers:
A<-boxplot(cal_cons$Weight.gained..grams.)
B<-boxplot(cal_cons$Calories.Consumed)

A$out    #No
B$out    #No

# Transformation of data :

x<-log(cal_cons$Weight.gained..grams.)
#x<-sqrt(cal_cons$Weight.gained..grams.)
qqnorm(x)
qqline(x)
mean(x)   #5.4921   
median(x) #5.2983
hist(x)

y<-log(cal_cons$Calories.Consumed)
qqnorm(y)
qqline(y)
hist(y)
mean(y) #7.711
median(y) #7.718

# Save the converted x and y data in a Data frame
cal_con1<-data.frame(weight=x,calories=y)   
View(cal_con1)
summary(cal_con1)
# Mean and median are almost equal

#Standardisation :
standard_cal_col <- as.data.frame(scale(cal_con1))
View(standard_cal_col)
summary(standard_cal_col)

#Normalization :
#normalize<-function(x)
#{
 # return((x-min(x))/(max(x)-min(x)))
#}
#standard_cal_col1<-as.data.frame(normalize(cal_con1))
#View(standard_cal_col1)
#summary(standard_cal_col1)

qqnorm(standard_cal_col$weight)
qqline(standard_cal_col$weight)

qqnorm(standard_cal_col$calories)
qqline(standard_cal_col$calories)

attach(standard_cal_col)

# Model Building

#Scatter Plot
plot(calories,weight)
cor(calories,weight)    #0.92  --- Strong

#Model1:

mod1<-lm(weight~calories,data=standard_cal_col)
summary(mod1)

#P value is less than 0.05, Intercept = -625.75236, Slope =0.42016
#Adjusted R Squared value = 0.8882 thatis greater than 8


mod1$coefficients
mod1$residuals

sqrt(sum(mod1$residuals^2)/nrow(standard_cal_col)) ## RMSE    0.3775442

# Prediction 
pred_conf <- as.data.frame(predict(mod1,interval="confidence"))
View(pred_conf)

#Prediction
Pred_val<-as.data.frame(predict(mod1,interval="predict"))

View(Pred_val)

# Model 2 for checking better R Squared Value :

mod2<-lm(weight~sqrt(calories),data=standard_cal_col)
summary(mod2)

#P value is less than 0.05, Intercept = -0.8566, Slope =1.9572
#Adjusted R Squared value = 0.9412 thatis greater than 8

mod2$coefficients
mod2$residuals

sqrt(sum(mod2$residuals^2)/nrow(standard_cal_col)) ## RMSE   0.09939147

#Prediction
pred_conf1 <- as.data.frame(predict(mod2,interval="confidence"))
View(pred_conf)

#Prediction
Pred_val1<-as.data.frame(predict(mod2,interval="predict"))
View(Pred_val1)

#Result : Model2 is having more R squared Value and less RMSE value. 
# Model2 is best fit for Prediction 



#Question 2

#Read a csv file :
del_time<-read.csv("C:\\Users\\admin\\Desktop\\python program files\\Assignment 3\\delivery_time.csv")
#del_time <- read.csv(file.choose())
View(del_time)
summary(del_time)

#Normality Check:
#Calculate Mean and median
mean(del_time$Delivery.Time)       #16.790
median(del_time$Delivery.Time)     #17.83
 
mean(del_time$Sorting.Time)        #6.190
median(del_time$Sorting.Time)      #6

#Graphical Representation

#QQ Plot :

qqnorm(del_time$Delivery.Time)
qqline(del_time$Delivery.Time)

qqnorm(del_time$Sorting.Time)
qqline(del_time$Sorting.Time)

#Histogram :
hist(del_time$Delivery.Time)

hist(del_time$Sorting.Time)

#Boxplot:
A<-boxplot(del_time$Delivery.Time)
B<-boxplot(del_time$Sorting.Time)

#Check if there is any outliers
A$out   #No
B$out   #No

#Data doesn't follows normal distribution

#Transformation of data :
#A<-sqrt(del_time$Delivery.Time)
#A<-log(del_time$Delivery.Time)
#mean(A)       #2.77
#median(A)     #2.88
#boxplot(A)
#qqnorm(A)
#qqline(A)

#Transformation doesn't make much difference so no need of Transformation.

attach(del_time)

#Scatter Plot


plot(Sorting.Time,Delivery.Time,main="Scatter Plot", col="Dodgerblue4", 
     col.main="Dodgerblue4", col.lab="Dodgerblue4", 
     xlab="Sorting Time", 
     ylab="Delivery Time", pch=20)

#plot(Sorting.Time,Delivery.Time)

#Model Building

#Model 1:

delivery_time <-lm(Delivery.Time~Sorting.Time,data=del_time)
summary(delivery_time)

#Pvalue is less than 0.05, Intercept=6.5827,Slope=1.6490
#Adjusted R Squared Value 0.6655 ..... Moderate



cor(Sorting.Time,Delivery.Time)    #0.82599  --- cor>0.85, Moderate

delivery_time$coefficients    
delivery_time$residuals

#RMSE 
sqrt(sum(delivery_time$residuals^2)/nrow(del_time))   #2.79165

#Confint and Prediction Check:

confint(delivery_time,level=0.95)
predict1<-predict(delivery_time,interval="predict")
View(predict1)




# Model 2 for checking better R Squared Value :

delivery_time1 <-lm(Delivery.Time~sqrt(Sorting.Time),data=del_time)
summary(delivery_time1)

#Pvalue is less than 0.05, Intercept=-2.518837 ,Slope=7.936591 
#Adjusted R Squared Value 0.6798 ..... Moderate

cor(Sorting.Time,sqrt(Delivery.Time))    #0.8390  --- cor>0.85, Moderate

delivery_time1$coefficients    
delivery_time1$residuals

#RMSE 
sqrt(sum(delivery_time1$residuals^2)/nrow(del_time))   #2.731543 

#Confint and Prediction Check:

confint(delivery_time1,level=0.95)
predict2<-predict(delivery_time1,interval="predict")
View(predict2)

# Model 3 for checking better R Squared Value :

delivery_time2 <-lm(log(Delivery.Time)~Sorting.Time,data=del_time)
summary(delivery_time2)

#Pvalue is less than 0.05, Intercept=2.12137 ,Slope=0.10555 
#Multiple R-squared:  0.7109,	Adjusted R-squared:  0.6957 ... Moderate

cor(log(Delivery.Time),Sorting.Time)   #0.8431

delivery_time2$coefficients    
delivery_time2$residuals

#RMSE 
sqrt(sum(delivery_time2$residuals^2)/nrow(del_time))   #0.166 

 # Model 3 for checking better R Squared Value :

delivery_time3<-lm(log(Delivery.Time)~Sorting.Time+I(Sorting.Time*Sorting.Time),data=del_time)
summary(delivery_time3)


#Pvalue is less than 0.05, Intercept=1.699 ,Slope=0.265 
#Multiple R-squared:  0.7649,	Adjusted R-squared:  0.7387 ... Moderate

cor(log(Delivery.Time),(Sorting.Time*Sorting.Time))   #0.788

delivery_time3$coefficients    
delivery_time3$residuals

#RMSE 
sqrt(sum(delivery_time3$residuals^2)/nrow(del_time))   #0.166 

#Prediction :

pred_delivery<-predict(delivery_time3,interval="predict")


#Result : Model 3 is having better correlation, R squared value and less RMSE.
#Model 3 is best fit for predicting Delivery Time




#Question 3
#Read a csv file :

emp_data<-read.csv("C:\\Users\\admin\\Desktop\\python program files\\Assignment 3\\emp_data.csv")
#emp_data<-read.csv(file.choose())
View(emp_data)
summary(emp_data)

##Normality Check:

mean(emp_data$Salary_hike)            #1688.6
median(emp_data$Salary_hike)          #1675

mean(emp_data$Churn_out_rate)         #72.9
median(emp_data$Churn_out_rate)       #71

#Graphical Representation

qqnorm(emp_data$Salary_hike)
qqline(emp_data$Salary_hike)

qqnorm(emp_data$Churn_out_rate)
qqline(emp_data$Churn_out_rate)

hist(emp_data$Salary_hike)
hist(emp_data$Churn_out_rate)

C<-boxplot(emp_data$Salary_hike)
D<-boxplot(emp_data$Churn_out_rate)

#Outliers
C$out      #No
D$out      #No

#Data doesn't follows Normal Distribution

#Transformation of Data :

Salary<-log(emp_data$Salary_hike)

qqnorm(Salary)
qqline(Salary)

hist(Salary)

mean(Salary)      #7.43
median(Salary)    #7.42

C_O_R <- log(emp_data$Churn_out_rate)

qqnorm(C_O_R)
qqline(C_O_R)

hist(C_O_R)

mean(C_O_R)          #4.28
median(C_O_R)        #4.26


# Save the converted Salary and C_O_R data in a Data frame
emp_data1<-data.frame(salary_hike=Salary,churn_out_rate=C_O_R)   
View(emp_data1)
summary(emp_data1)

#Standardisation of data:

standard_emp_data<-as.data.frame(scale(emp_data1))
View(standard_emp_data)
summary(standard_emp_data)
attach(standard_emp_data)


#Scattar Plot:
plot(salary_hike,churn_out_rate)

#Model 1:
emp_model<-lm(churn_out_rate~salary_hike,data = standard_emp_data)
summary(emp_model) 

#Intercept -4.445916e-15  and  slope -9.429170e-01
#P<0.05
#Multiple R-squared:  0.8891,	Adjusted R-squared:  0.8752  .... strong

cor(salary_hike,churn_out_rate)    #-0.942917   ... Strong

emp_model$coefficients
emp_model$residuals

#RMSE
sqrt(sum(emp_model$residuals^2)/nrow(standard_emp_data))   #0.3159

#Prediction :
predict_emp<-predict(emp_model,interval = "predict")
View(predict_emp)
summary(predict_emp)

#Model 2 for better R Squared Value :

emp_model1<-lm(churn_out_rate~log(salary_hike),data=standard_emp_data)
summary(emp_model1)

#Intercept = -1.05279  and Slope = -0.29057 
#p<0.05
#Multiple R-squared:  0.9336,	Adjusted R-squared:  0.9115  ..... Strong
sal_log<-log(salary_hike)
cor(sal_log,churn_out_rate)      #NA

 #RMSE :
sqrt(sum(emp_model1$residuals^2)/nrow(standard_emp_data))   #0.0752


#Model 3 for better R Squared Value :

emp_model2<-lm(churn_out_rate~sqrt(salary_hike),data=standard_emp_data)
summary(emp_model2)

#Intercept = -0.04425  and Slope = -0.96040  
#p<0.05
#Multiple R-squared:  0.9917,	Adjusted R-squared:  0.9889  ..... Strong

cor(sqrt(sal_log),churn_out_rate)      #NA

#RMSE :
sqrt(sum(emp_model2$residuals^2)/nrow(standard_emp_data))   #0.0752


#Result :  Model 2 and Model 3 are having strong R squared values and less RMSE
#But Correlation is not clear.
#So Model 1 is best fit for predicting Churn Out rate.
plot(emp_model1)


#Question 4
#Read a csv file :
sal_data<-read.csv("C:\\Users\\admin\\Desktop\\python program files\\Assignment 3\\Salary_Data.csv")
#sal_data<-read.csv(file.choose())
View(sal_data)
summary(sal_data)

#Normality Check :

mean(sal_data$YearsExperience)    #5.313
median(sal_data$YearsExperience)  #4.7

mean(sal_data$Salary)             #76003
median(sal_data$Salary)           #65237

#Graphical Representation:

#QQ Plot :
qqnorm(sal_data$YearsExperience)
qqline(sal_data$YearsExperience)

qqnorm(sal_data$Salary)
qqline(sal_data$Salary)

#Histogram
hist(sal_data$YearsExperience)
hist(sal_data$Salary)

#Data doesn't follows Normal Distribution.

#Box Plot to check Outliers :

A<-boxplot(sal_data$YearsExperience)
B<-boxplot(sal_data$Salary)

A$out           #No 
B$out           #No

#Transformation of data :

YOE<-sqrt(sal_data$YearsExperience)
mean(YOE)       #2.21
median(YOE)     #2.16
hist(YOE)
#YOF<-log(sal_data$YearsExperience)
boxplot(YOE)

sal<-log(sal_data$Salary)
#sal<-sqrt(sal_data$Salary)

mean(sal)       #11.17
median(sal)     #11.08
hist(sal)
boxplot(sal)

# Save the converted YOE and sal data in a Data frame
sal_data_main<-data.frame(experience=YOE,salary=sal)
View(sal_data_main)
summary(sal_data_main)

#Standardisation of data :
salary_data<-as.data.frame(scale(sal_data_main))
View(salary_data)
summary(salary_data)
attach(salary_data)

#Scattar Plot:
plot(experience,salary)

cor(experience,salary)      #0.971  ... Strong

#Model 1:

salary_model<-lm(salary~experience,data=salary_data)
summary(salary_model)

#p<0.05, Intercept = -1.990e-15 and slope = 9.713e-01
#Multiple R-squared:  0.9434,	Adjusted R-squared:  0.9413  .... Strong

#RMSE :

sqrt(sum(salary_model$residuals^2)/nrow(salary_data))  #0.2339

#Prediction Value:
 
Pred_hike<-predict(salary_model,interval="predict")
View(Pred_hike)

#Model 2 for better R squared Value:

salary_model1<-lm(salary~sqrt(experience),data=salary_data)
summary(salary_model1)

#p<0.05, Intercept = -0.3190 and slope = 1.3943
#Multiple R-squared:  0.9135,	Adjusted R-squared:  0.9063

#RMSE :

sqrt(sum(salary_model1$residuals^2)/nrow(salary_data))  #0.0929

cor(sqrt(experience),salary)

#Prediction Value:

Pred_hike1<-predict(salary_model1,interval="predict")
View(Pred_hike1)



#Result : Model 2 is having less RMSE value but Correlation is not clear. 
#So Model 1 is best fit for prediction Salary Hike