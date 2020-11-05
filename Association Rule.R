#Association Rules:

#Q1.Prepare rules for the data set Book :

#Read a csv file:

book<-read.csv(file.choose())
str(book)
View(book)

#Convert into factors :

book$ChildBks<-as.factor(book$ChildBks)
book$YouthBks<-as.factor(book$YouthBks)
book$CookBks<-as.factor(book$CookBks)
book$DoItYBks<-as.factor(book$DoItYBks)
book$RefBks<-as.factor(book$RefBks)
book$ArtBks<-as.factor(book$ArtBks)
book$GeogBks<-as.factor(book$GeogBks)
book$ItalCook<-as.factor(book$ItalCook)
book$ItalAtlas<-as.factor(book$ItalAtlas)
book$ItalArt<-as.factor(book$ItalArt)
book$Florence<-as.factor(book$Florence)

str(book)


#Install arules package :

install.packages("arules")
library(arules)

# Creating rules by apriori algo. :
rules_books<-apriori(book)

#Confidence=0.8, support=0.1
# Minimum length is 1 and max length is 10
#set of rules =13987

inspect(head(rules_books),n=10)
rules_sorted<-inspect(head(sort(rules_books,by="lift"),n=10))

#lhs              rhs          support confidence coverage     lift    count
#{ItalCook=1}  => {CookBks=1}   0.1135  1.0000000   0.1135   2.320186   227

#{YouthBks=1,                                                            
#CookBks=1,                                                             
#ItalArt=0,                                                             
#Florence=0}  => {ChildBks=1}  0.1025  0.8070866   0.1270    1.908006   205

#{DoItYBks=1,                                                            
#ArtBks=1}    => {CookBks=1}   0.1015  0.8218623   0.1235   1.906873    203

#{CookBks=1,                                                             
#RefBks=1,                                                              
#ItalAtlas=0} => {ChildBks=1}  0.1000  0.8064516   0.1240  1.906505     200

#{DoItYBks=1,                                                            
#GeogBks=1,                                                             
#ItalAtlas=0} => {CookBks=1}   0.1010  0.8211382   0.1230  1.905193    202

#{DoItYBks=1,                                                            
#GeogBks=1}   => {CookBks=1}   0.1085  0.8188679   0.1325  1.899926   217

#{CookBks=1,                                                             
#RefBks=1}    => {ChildBks=1}  0.1225  0.8032787   0.1525  1.899004   245

#{YouthBks=1,                                                            
#CookBks=1,                                                             
#Florence=0}  => {ChildBks=1}  0.1140  0.8028169   0.1420  1.897912   228

#{ArtBks=1,                                                              
#GeogBks=1}   => {ChildBks=1}  0.1020  0.8000000   0.1275  1.891253   204

#{YouthBks=1,                                                            
#CookBks=1,                                                             
#ItalArt=0}   => {ChildBks=1}  0.1140  0.8000000   0.1425  1.891253   228


#Reduce the no of rules by changing support and confidence value:

rules_book<-apriori(book,parameter = list(support=0.2,confidence=0.5,minlen=5))

#Set of rules : 9407
#Confidence=0.5 and support=0.2

rules_sorted1<-inspect(head(sort(rules_book,by="lift"),n=10))

quality(head(sort(rules_book,by="lift")))

#support confidence coverage     lift     count
# 0.353  0.7853170   0.4495    1.380171   706
# 0.353  0.7853170   0.4495    1.380171   706
# 0.353  0.7853170   0.4495    1.380171   706
# 0.353  0.7853170   0.4495    1.380171   706
# 0.313  0.7815231   0.4005    1.373503   626
# 0.313  0.7815231   0.4005    1.373503   626



#Visualization of Rules created:
#Need to install package arulesViz
install.packages("arulesViz")
library(arulesViz)

#Scatter Plot
plot(sort(rules_book,by="lift"))
plot(sort(rules_books,by="lift"))

#Graph Plot:
plot(sort(rules_book,by="lift"),method="graph")
plot(sort(rules_books,by="lift"),method="graph")

#Grouped Matix for top 10 rules :

plot(head(sort(rules_book,by="lift"),n=100),method = "grouped")
plot(head(sort(rules_books,by="lift"),n=100),method = "grouped")

#Result:

#Those who are interested in childbks also shows their interest in Cookbks and Youth Books



#Q2. Prepare rules for data set groceries:

#Read csv file: 
groceries<-read.csv(file.choose())
View(groceries)
str(groceries)

#Groceries data set contains transactions done in store.
#Read this data set as transaction

groceries_trans<-read.transactions(file.choose(),sep=",")
summary(groceries_trans)

#most frequent items:
#whole milk    other vegetables      rolls/buns        soda 
#   2513             1903             1809             1715 
# yogurt          (Other) 
# 1372            34055 

#Check for first 10 transcations:
 head(groceries,n=10)

#Creating rules by Apriori Algo. :
 library(arules)
rules_groceries<-apriori(groceries_trans) 

#Confidence=0.8, support=0.1,minlen=1
#Set of rules : 0

#Change support and confidence value for better rules:

rules_groceries1<-apriori(groceries_trans,parameter = list(support=0.002,confidence=0.5,minlen=2))
#confidence=0.5, Support=0.002, minlen=2
#Set of rules: 1098

# Check first five rules- sort by lift:
inspect(head(sort(rules_groceries1,by="lift")))

#     lhs                    rhs               0support     confidence  coverage    lift   count
#[1] {butter,                                                                                     
#  hard cheese}       => {whipped/sour cream} 0.002033554  0.5128205 0.003965430 7.154028    20
#[2] {beef,                                                                                       
#  citrus fruit,                                                                               
#  other vegetables}  => {root vegetables}    0.002135231  0.6363636 0.003355363 5.838280    21
#[3] {citrus fruit,                                                                               
#  other vegetables,                                                                           
#  tropical fruit,                                                                             
#  whole milk}        => {root vegetables}    0.003152008  0.6326531 0.004982206 5.804238    31
#[4] {citrus fruit,                                                                               
#  frozen vegetables,                                                                          
#  other vegetables}  => {root vegetables}    0.002033554  0.6250000 0.003253686 5.734025    20
#[5] {beef,                                                                                       
#  other vegetables,                                                                           
#  tropical fruit}    => {root vegetables}    0.002745297  0.6136364 0.004473818 5.629770    27
#[6] {bottled water,                                                                              
#  root vegetables,                                                                            
#  yogurt}            => {tropical fruit}     0.002236909  0.5789474 0.003863752 5.517391    22

#Result:

# As per first rule, People who buy  butter and Hard Cheese are also  interested in buying Whipped/Sour Cream.
#As per 2nd,3rd,4th and 5th rule, people who buy citrus fruit, other vegetables and whole milk are also interested in root vegetables.

#As we know that, most frequent item is whole milk.
#Check for the rule, lhs containing whole milk

rules_wholemilk<-apriori(groceries_trans,parameter=list(support=0.002,confidence=0.1,minlen=2),appearance = list(lhs="whole milk"))
#Set of rules=17

inspect(rules_wholemilk)

#Check for the rule, rhs containing whole milk

rules_wholemilk_1<-apriori(groceries_trans,parameter = list(support=0.001,confidence=0.1,minlen=2),appearance = list(lhs="whole milk"))
#Set of rules : 17

inspect(rules_wholemilk_1)

#People who buy milk are more interested in curd, butter,vegetables etc

#Visual Representation:

library(arules)

itemFrequencyPlot(groceries_trans,topN=20)
#Whole milk and other vegetables are having high frequencies

#Scatter Plot
plot(rules_groceries1)
plot(rules_wholemilk)
plot(rules_wholemilk_1)

#Graph Plot
plot(rules_groceries1,method="graph")
plot(rules_wholemilk,method="graph")
plot(rules_wholemilk_1,method="graph")

#grouped matrix:
plot(rules_groceries1,method="grouped")

#There are more rules associated with whole milk and other vegetables.


#Q3.
#Prepare rules for the data set my movies

#Read a csv file:
movies<-read.csv(file.choose())
View(movies)
str(movies)

#Convert into factors :

movies$Sixth.Sense<-as.factor(movies$Sixth.Sense)
movies$Gladiator<-as.factor(movies$Gladiator)
movies$LOTR1<-as.factor(movies$LOTR1)
movies$Harry.Potter1<-as.factor(movies$Harry.Potter1)
movies$Patriot<-as.factor(movies$Patriot)
movies$LOTR2<-as.factor(movies$LOTR2)
movies$Harry.Potter2<-as.factor(movies$Harry.Potter2)
movies$LOTR<-as.factor(movies$LOTR)
movies$Braveheart<-as.factor(movies$Braveheart)
movies$Green.Mile<-as.factor(movies$Green.Mile)


str(movies)

#Creating rules by apriori algo:
library(arules)
rules_movies<-apriori(movies)
#Set of rules: 1384344
#Confidence=0.8,support=0.1,minlen=1

inspect(head(sort(rules_movies,by="lift")))

#lhs                   rhs                support confidence coverage lift
# {V3=Gladiator}     => {V2=LOTR}          0.1     1          0.1      10  
# {V2=LOTR}          => {V3=Gladiator}     0.1     1          0.1      10  
# {V2=LOTR1}         => {V3=Harry Potter1} 0.1     1          0.1      10  
# {V3=Harry Potter1} => {V2=LOTR1}         0.1     1          0.1      10  
# {V2=LOTR1}         => {V5=LOTR2}         0.1     1          0.1      10  
# {V5=LOTR2}         => {V2=LOTR1}         0.1     1          0.1      10  


#Change the values for support and confidence to reduce the no of rules:

rules_movies1<-apriori(movies,parameter = list(support=0.005,confidence=0.5,minlen=2,maxlen=3))
#Set of rules=1125007
inspect(head(sort(rules_movies1,by="lift")))

#lhs                   rhs                support confidence coverage lift
#{V3=Gladiator}     => {V2=LOTR}          0.1     1          0.1      10  
#{V2=LOTR}          => {V3=Gladiator}     0.1     1          0.1      10  
#{V2=LOTR1}         => {V3=Harry Potter1} 0.1     1          0.1      10  
#{V3=Harry Potter1} => {V2=LOTR1}         0.1     1          0.1      10  
#{V2=LOTR1}         => {V5=LOTR2}         0.1     1          0.1      10  
#{V5=LOTR2}         => {V2=LOTR1}         0.1     1          0.1      10  

#visual Representation:

library(arulesViz)

#Item frequency plot:

itemFrequencyPlot(as(movies[1:5],"transactions"))

#Scatter Plot

plot(sort(rules_movies,by="lift"))
plot(sort(rules_movies1,by="lift"))

#Graph Plot:
plot(head(sort(rules_movies,by="lift"),n=20),method="graph")
plot(head(sort(rules_movies1,by="lift"),n=20),method="graph")

#Grouped Matrix plot:

plot(head(sort(rules_movies,by="lift"),n=20),method="grouped")
plot(head(sort(rules_movies1,by="lift"),n=20),method="grouped")

#Result:
#Those who watched LOTR also watched Gladiator.
#Those who watched Harry also watched LOTR2 and Sixth sense