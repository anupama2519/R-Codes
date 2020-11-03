
#Q1. 
#Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.
 
#Read csv file:

#crime<-read.csv(file.choose())
crime<-read.csv("C:\\Users\\admin\\Desktop\\python program files\\Assignment 7\\crime_data.csv")
View(crime)
#class(crime)

crime<-crime[,2:5]      #Excluding the Name of Places.
View(crime)

#Preprocessing of data :

#Calculate Mean and Median :

summary(crime)

#    Murder          Assault        UrbanPop              Rape
#Median : 7.250   Median :159.0   Median :66.00      Median :20.10
#Mean   : 7.788   Mean   :170.8   Mean   :65.54      Mean   :21.23

#Graphical Representation :

#Histogram:
hist(crime$Murder)
hist(crime$Assault)
hist(crime$UrbanPop)
hist(crime$Rape)

#QQ Plot:
qqnorm(crime$Murder)
qqline(crime$Murder)

qqnorm(crime$Assault)
qqline(crime$Assault)

qqnorm(crime$UrbanPop)
qqline(crime$UrbanPop)

qqnorm(crime$Rape)
qqline(crime$Rape)

#Box Plot:

A<-boxplot(crime$Murder)
B<-boxplot(crime$Assault)
C<-boxplot(crime$UrbanPop)
D<-boxplot(crime$Rape)

#Outliers :

A$out     #No
B$out     #No
C$out     #No
D$out     #44.5 and 46.0

#Only Rape variable has outliers.

#All the variables follows Normal Distribution(Approx) except Assault.

#Checking for Null Values:

is.na(crime$Murder)      #False
is.na(crime$Assault)     #False
is.na(crime$UrbanPop)    #False
is.na(crime$Rape)        #False

# No Null Values in Data set Crime

# Standardisation of Data:
#This process is better than Normalization because it deals with outliers.

crime_standard<-scale(crime)
View(crime_standard)
summary(crime_standard)
class(crime_standard)

#Calculating Distance (Euclidean Method) :
#Distance Matrix

dist<-dist(crime_standard,method = "euclidean")
str(dist)

#Creating Clusters :

crime_clust<-hclust(dist,method = "complete")

#Plot a Diagram(Dendrogram):

plot(crime_clust)
plot(crime_clust,hang = -1) #hang=-1 is used to show the labels clearly below the plot 

#Draw rectangle around clusters for clear view:

rect.hclust(crime_clust,k=3,border="red")  #k=No. of clusters

# For k=3, Size of each cluster is not same. 

rect.hclust(crime_clust,k=4,border = "blue")
#Size is almost similar. 

#Cut tree into 4 clusters.

groups<-cutree(crime_clust,k=4)
class(groups)

#Add groups column to crime data set
crime_data_final<-cbind(crime,groups)
View(crime_data_final)

#Sort the crime data by groups

?aggregate()
crime_final<-aggregate(crime_data_final,by=list(crime_data_final$groups),FUN = mean)

#Group.1     Murder       Assault     UrbanPop       Rape     groups
#   1      14.087500    252.7500     53.50000      24.53750      1
#   2      11.054545    264.0909     79.09091      32.61818      2
#   3      5.871429     134.4762     70.76190      18.58095      3
#   4      3.180000     78.7000      49.30000      11.63000      4

write.csv(crime_data_final,file = "C:\\Users\\admin\\Desktop\\python program files\\Assignment 7\\crime_final.csv")

#Result :
#We can say that Group 2 is having higher rate of crime and Group 4 is having less rate of crime.



#Q2.
#Perform clustering (Both hierarchical and K means clustering) for the 
#airlines data to obtain optimum number of clusters. 
#Draw the inferences from the clusters obtained.

#Read csv file:

library(readxl)

airlines<-read_xlsx("C:\\Users\\admin\\Desktop\\python program files\\Assignment 7\\EastWestAirlines.xlsx",sheet = "data")
#airlines<-read_xlsx(file.choose())
View(airlines)
colnames(airlines)  #To check the column names
ncol(airlines)      #To check column numbers

airlines<-airlines[,2:12]   ##Excluding the ID Column
View(airlines)

#Preprocessing of Data: 

#Check for null values:

is.na(airlines$Balance)                   #False
is.na(airlines$Qual_miles)                #False
is.na(airlines$cc1_miles)                 #False
is.na(airlines$cc2_miles)                 #False
is.na(airlines$cc3_miles)                 #False
is.na(airlines$Bonus_miles)               #False
is.na(airlines$Bonus_trans)               #False
is.na(airlines$Flight_miles_12mo)         #False
is.na(airlines$Flight_trans_12)           #False
is.na(airlines$Days_since_enroll)         #False
is.na(airlines$`Award?`)                  #False

# No Null Values in Data set EastWestAirlines

#Graphical Representation:

hist(airlines$Balance)
hist(airlines$Qual_miles)
hist(airlines$cc1_miles)
hist(airlines$cc2_miles)
hist(airlines$cc3_miles)
hist(airlines$Bonus_miles)
hist(airlines$Bonus_trans)
hist(airlines$Flight_miles_12mo)
hist(airlines$Flight_trans_12)
hist(airlines$Days_since_enroll)

#Boxplot :
A<-boxplot(airlines$Balance,horizontal = T)
B<-boxplot(airlines$Qual_miles,horizontal = T)
C<-boxplot(airlines$cc1_miles)
D<-boxplot(airlines$cc2_miles,horizontal = T)
E<-boxplot(airlines$cc3_miles,horizontal = T)
F<-boxplot(airlines$Bonus_miles,horizontal = T)
G<-boxplot(airlines$Bonus_trans)
H<-boxplot(airlines$Flight_miles_12mo)
I<-boxplot(airlines$Flight_trans_12)
J<-boxplot(airlines$Days_since_enroll)

#This data set is having lots of outliers in it.

#Handling Outliers :

#Function to remove the outliers based on Interquartile Range :

outlierTreament<-function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)}

#Column Balance:
Balance<-outlierTreament(airlines$Balance)
A1<-boxplot(Balance)

#Column Qual_miles:
Qual_miles<-outlierTreament(airlines$Qual_miles)
B1<-boxplot(Qual_miles)
View(Qual_miles)

#Column Bonus_miles:
Bonus_miles<-outlierTreament(airlines$Bonus_miles)
F1<-boxplot(Bonus_miles)

#Column Bonus_trans:
Bonus_trans<-outlierTreament(airlines$Bonus_trans)
G1<-boxplot(Bonus_trans)

#Column Flight_miles_12mo:
Flight_miles_12mo<-outlierTreament(airlines$Flight_miles_12mo)
H1<-boxplot(Flight_miles_12mo)

#Column Flight_trans_12:
Flight_trans_12<-outlierTreament(airlines$Flight_trans_12)
I1<-boxplot(Flight_trans_12)

#Combined all the columns without Outliers:

airlines_new<-cbind(Balance,Qual_miles,"cc1_miles"=airlines$cc1_miles,"cc2_miles"=airlines$cc2_miles,"cc3_miles"=airlines$cc3_miles,Bonus_miles,Bonus_trans,Flight_miles_12mo,Flight_trans_12,"Days_since_enroll"=airlines$Days_since_enroll,"Award"=airlines$`Award?`)
View(airlines_new)
ncol(airlines_new)


#Calculating Mean and Median

summary(airlines_new)

#   Balance         Qual_miles      cc1_miles      cc2_miles       cc3_miles    
#Median : 43097   Median :  0.0   Median :1.00   Median :1.000   Median :1.000  
#Mean   : 65602   Mean   : 41.8   Mean   :2.06   Mean   :1.015   Mean   :1.012  

#Bonus_miles     Bonus_trans    Flight_miles_12mo      Flight_trans_12
#Median : 7171   Median :12.00   Median :   0.0        Median :0.000  
#Mean   :15845   Mean   :11.33   Mean   : 427.3        Mean   :1.369  

#Days_since_enroll        Award       
#Median :4096         Median :0.0000  
#Mean   :4119         Mean   :0.3703  


#Standardisation of Data :

airlines_standard<-scale(airlines_new)
View(airlines_standard)

summary(airlines_standard)

#Hierarchical Method to find the number of Clusters:

#Calculating Distance (Euclidean Method) :
#Distance Matrix
dist_airlines<-dist(airlines_standard,method="euclidean")
str(dist_airlines)

#Creating Clusters :
airlines_clust<-hclust(dist_airlines,method="complete")
str(airlines_clust)

#Plot a Diagram(Dendrogram):

plot(airlines_clust)
plot(airlines_clust,hang = -1) #hang=-1 is used to show the labels clearly below the plot 

#Draw rectangle around clusters for clear view:

rect.hclust(airlines_clust,k=3,border="red")  #k=No. of clusters

# For k=3, Size of each cluster is not same. 

rect.hclust(airlines_clust,k=4,border = "blue")
#Size of each cluster is not similar.

rect.hclust(airlines_clust,k=5,border="pink")

#Cut tree into 5 clusters.

groups<-cutree(airlines_clust,k=5)
class(groups)

#Add groups column to crime data set
airlines_final<-as.data.frame(cbind(airlines_new,groups))
View(airlines_final)
class(airlines_final)


#Sort the data by groups:

airlines_sorted<-aggregate(airlines_final,by=list(airlines_final$groups),FUN=mean)
str(airlines_sorted)

#Group.1   Balance     Qual_miles   cc1_miles   cc2_miles    cc3_miles   Bonus_miles
#   1       50391.48    29.86337    1.497558    1.000000     1.000977     7537.38
#   2       119048.33   85.01149    4.064368    1.000000     1.000000     44464.70
#   3       57104.85    17.20000    1.139535    2.348837     1.000000     14554.01
#   4       115682.00   184.90000   3.750000    1.000000     3.000000     60136.05
#   5       100133.29   0.00000     3.363636    1.000000     4.454545     60560.51

#Group  Bonus_trans  Flight_miles_12mo Flight_trans_12   Days_since_enroll     Award groups
# 1     8.760013        325.0893        1.115598          3921.845            0.2745034      1
# 2     19.881609       778.7484        2.231034          4811.794            0.7045977      2
# 3     16.511628       534.7488        1.906977          3968.930            0.3953488      3
# 4     29.000000       1264.4500       2.500000          5058.750            0.5000000      4
# 5     25.363636       425.3545        1.272727          4452.091            0.5454545      5

table(groups)

#  1    2    3     4    5 
#3071  870   43    4   11 

#Result :
#Cluster 2 : is having maximum persons that had award Flights or free flights.


#Since we are dealing with larger data set, Dendrogram is not clear.


#K Means Method to find the Number of Clusters:

#Apply K means for k=5 clusters
airlines_clust_K<-kmeans(airlines_standard,5)    # Randomly takes k=5
str(airlines_clust_K)

#We can see there are  5 clusters size of -  692 1473 547 583 704
#cluster     : int [1:3999]
#Within Cluster sum of squares :
#2402 3393 6804 6777 6081


#Calculate the center location of the cluster:
airlines_clust_K$centers  #A Matrix of cluster centers

#Elbow curve to find the number of clusters:
airlines_clust_K$withinss
twss = c()
for (i in 2:15) twss[i] = sum(kmeans(airlines_standard, centers=i)$withinss)

plot(1:15, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
View(twss)

#Result :From Elbow curve, after 3 the drop is constant.. so we can say that No of clusters are 3 (k=3)

#Apply K Means again for k=3
airlines_k<-kmeans(airlines_standard,3)
str(airlines_k)

#Append Cluster into Airlines Data set:

airlines_final_k<-as.data.frame(cbind(airlines_new,"Groups"=airlines_k$cluster))
View(airlines_final_k)

#Sort the data set by Groups:

airlines_Kmeans_final<-aggregate(airlines_final_k,by=list(airlines_final_k$Groups),FUN=mean)

#Group.1   Balance    Qual_miles    cc1_miles cc2_miles cc3_miles Bonus_miles Bonus_trans   Flight_miles_12mo
#   1     38001.00    24.70824      1.181737  1.018708  1.000445    4018.804    6.429399        83.28272
#   2     104247.91   134.12435     2.207254  1.024180  1.008636   22774.869   17.531952        2359.67893
#   3     99295.49    28.95455      3.663830  1.001702  1.036596   35025.744   17.633191        132.22749
#Flight_trans_12    Days_since_enroll     Award     Groups
#   0.3207127         3645.807          0.1786192      1
#   7.3696028         4586.408          0.6856649      2
#   0.4136170         4791.279          0.5812766      3



#For larger data set, Using Clara function to find clusters
install.packages("cluster")
library(cluster)

airlines_clara<-clara(airlines_standard,3)
str(airlines_clara)

#Plot 
clusplot(airlines_clara,color=TRUE,SHADE=TRUE)   #Var = 47.36

#Result:

#Cluster 2 is having highest number of people with free award flight.
#Cluster 1 is having least number of people eith free award flight.