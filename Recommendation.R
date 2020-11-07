#Recommendation:
#Q1.    Recommend a best book based on the ratings.

#Install required packages for Recommendation :
install.packages("recommenderlab",dependencies = TRUE)
install.packages("Matrix")
install.packages("caTools")
library(recommenderlab)
library(Matrix)
library(caTools)

#Read CSV file :
book_rating<-read.csv(file.choose())
View(book_rating)

#First column is S.No. ,so delete Column 1:
book_rating<-book_rating[,-1]
View(book_rating)

str(book_rating)
dim(book_rating)   #10000 rows  with  3 variables

head(book_rating)  #check for first five rows 

plot(book_rating)

#Preprocessing of data :

#Check for NULL Values:
unique(is.na(book_rating))

#There are no NULL values

#Check for Names of Book Title:
unique(book_rating$Book.Title)
#There are 9659 unique books name.

#Check for rating scale:
unique(book_rating$Book.Rating)

#Ratings are from 1 - 10

#Check for Outliers:

boxplot(book_rating$User.ID)                    #No
boxplot(book_rating$Book.Rating,horizontal = T) 
# Most of the books are rated from 4 - 10
#Only few books are rated between 1-3

#Rating distribution:
hist(book_rating$Book.Rating)
# Not a Normal Distribution.
#Mostly books are rated in between  6-8

#Convert the data in realRatingMatrix to build recommendation model:
book_rating_matrix<-as(book_rating,"realRatingMatrix")
dim(book_rating_matrix)   #2182 9659

#Model Building :

#We will create recommendation model based on :

#Popularity based

model_popular_book<-Recommender(book_rating_matrix,method="POPULAR")
summary(model_popular_book)

#Prediction :

pred_model_popular_book<-predict(model_popular_book,book_rating_matrix[2],n=5)
as(pred_model_popular_book,"list")

# "In the Beauty of the Lilies"        "Black House"                
# "White Oleander : A Novel"           "The Magician's Tale"        
# "Nowle's Passing: A Novel"  

#Item based collaborative filtering
#Books recommended for specific users simliar to a specified item or item chosen by user.

#model_item_book<-Recommender(book_rating_matrix,method="IBCF")

#Prediction:
#pred_model_item_book<-predict(model_item_book,book_rating_matrix[2],n=5)
#as(pred_model_item_book,"list")

#User based collaborative filtering
#Recommends items that are similar purchased by the same people.

model_user_book<-Recommender(book_rating_matrix,method="UBCF")

#Prediction:

pred_model_user_book<-predict(model_user_book,book_rating_matrix[2],n=5)
as(pred_model_user_book,"list")

# "'48"                                                                  
# "'O Au No Keia: Voices from Hawai'I's Mahu and Transgender Communities"
# " Jason, Madison &amp"                                                 
# " Other Stories;Merril;1985;McClelland &amp"                           
# " Repairing PC Drives &amp"   

#Result : User based collaborative Filtering gives better result.