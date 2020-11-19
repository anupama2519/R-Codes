
#Text Mining

#1.Extract reviews of any product from ecommerce website like snapdeal and amazon
#2) Perform sentimental analysis

#Load the required packages:

#install.packages("rvest")
library(rvest)
#install.packages("XML")
library(XML)
#install.packages("magrittr")
library(magrittr)
library(tm)
library(wordcloud)
#library(wordcloud2)
#install.packages("syuzhet")
library(syuzhet)
#install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
#library(dplyr)

######################## Amazon Reviews of a book #############################
link <- "https://www.amazon.in/product-reviews/8192910962/ref=acr_dpproductdetail_text?ie=UTF8&showViewpoints=1"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(link,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)   #100

#Write the movie reviews in table and save it in Local repository:
getwd()
setwd("C:/Users/admin/Desktop/python program files")
write.table(amazon_reviews,"amazon_Reviews.txt",row.names = F)

#Read the file:
reviews_amazon <- read.delim('amazon_Reviews.txt')
View(reviews_amazon)
str(reviews_amazon)    #100 obs. of  1 variable
class(reviews_amazon)   #data.frame

#Lets understand the data :

head(reviews_amazon)

#1\n\n\n\n\n\n\n\n\n\n  \n  \n    \n  SPOILERS AHEAD PEOPLE!!Murphy's idea is 
   #simple. He says if you want something, just embed the idea into your non-resistive
   #(Subconscious) mind.To be honest,in the beginning of the book i did not find the 
   #whole idea very logical. It seemed like somebody is trying to sell some BS. But then
   #a realization came upon me...He says\\By embedding the idea into our sub-conscious 
   #we can get anything\\. But, how???Now i feel i know how, as we force the idea into 
   #our mind we involuntarily start working towards it with our greatest capability using
   #our full potential and thus we have achieved what seemed initially impossible to us.
   #All in all, this book was really helpful to me(although i am nearly living the best
   #years of my life). If it can help a person who is really satisfied with his life,
   #i have no idea how many wonders it will
   #do to those people life who want something really bad and need anything to help them 
   #with their goal.\n\n  \n
#2\n\n\n\n\n\n\n\n\n\n  \n  \n    \n  Let me tell you about what the book says: 
   #Your Mind Has two parts, If you direct the conscious one to be positive , 
   #the subconscious one will bring happiness in your life.Now write it down in 
   #3000 ways, explaining it. Again , Again, Again,No noAgain, then Again..Oh GOddd
   #....put out some god, bible, Wife, wealth Money....I mean Again....did you hear 
   #it?? Againthen Again.Yes. You just wrote this book. Congrats, go to a publisher 
   #and sell it through amazon.[Before End] : If this does not makes you rich, 
   #let me give you a suggestion from this book. Every night before sleep, 
   #just whisper : W-e-a-l-t-h....w..e..a..l..t..h.....You will sure become rich.
   #\n\n  \n
#3\n\n\n\n\n\n\n\n\n\n  \n  \n    \n  Very useful book...good for maintaining a calm 
   #mental health and it contains so many inspirational incidents where the power of 
   #subconscious mind changed the life of so many persons...must read book...highly 
   #recommended for youngsters to build a good future....\n\n  \n
#4\n\n\n\n\n\n\n\n\n\n  \n  \n    \n  I have no idea why people buy this one. I
   #bought it reading the reviews. But this book is a waste of money. Only book I 
   #could never finish reading. The same thing is written chapter after chapter. 
   #Learn from the author how to stretch a one page idea to a 350+ page book. Also 
   #most of the incidences are too supernatural that they are coincidental and not 
   #scientific unlike what the book claims.I'm a book lover and this is the only 
   #book I regrer buying.\n\n  \n

#5 \n\n\n\n\n\n\n\n\n\n  \n  \n    \n  Having read so many books & with technology 
   #being a click away, mostly people would have seen videos on other platforms to 
   #understand the making of mind & it's nuisances. If you're reading it for first 
   #time or the subject is new to you, you will be spellbound <U+0001F60D> If not 
   #then<U+0001F910> ( Hope my reading experience helps )<U+23E9>The Power of Your 
   #Subconscious Mind is simply written and tries to be free of culture or religion.
   #<U+23E9> It is slightly repetitive, but this in itself mirrors the book's idea of 
   #subconscious programming.<U+23E9>For full effect, the author's advice that it be 
   #read at least twice - Isn't that time consuming ?<U+23E9>It is also sometimes a bit
   #whimsical and illogical - a bit like the subconscious itself, but Murphy's whole 
   #point, in fact, is that if you refuse to try to understand the non-rational mind, 
   #your rational desires and plans will be forever sabotaged.<U+23E9>The first half of 
   #the book is the best, as it explains how the subconscious works.<U+23E9>The second 
   #half deals with its role and power to transform in areas like marriage, human 
  #, scientific discovery, sleep, fear, forgiveness and 'eternal youth'.<U+23E9>For 
   #some the book will be a bit 'way out', but for first time readers it can be an 
   #awakening call<U+23F0>\n\n  \n


#From the above text , we can figure out that In text column,there are lots of 
#abbreviations,internet slangs and numbers are used. 
#So, it is needed to clean the data before processing further.

#To do this, We have to:
# remove the numbers
#remove the punctuations
#convert the data in Lower case
#remove the stop words
#Manage the white spaces

#Data Cleaning :

#create the corpus :

reviews_corpus_amazon <-Corpus(VectorSource(reviews_amazon))
print(reviews_corpus_amazon)
#Metadata:  corpus specific: 1, document level (indexed): 0
#Content:  documents: 1
class(reviews_corpus_amazon)   

#Inspect top five corpus data :
inspect(reviews_corpus_amazon[1:5])

#Convert into lower case :

reviews_corpus_amazon<-tm_map(reviews_corpus_amazon,tolower)
inspect(reviews_corpus_amazon[1:3])         #Changed to lower case

#Remove the numbers :
reviews_corpus_amazon<-tm_map(reviews_corpus_amazon,removeNumbers)
inspect(reviews_corpus_amazon[1:5])          #Numbers are removed

#Remove Punctuations :
reviews_corpus_amazon<-tm_map(reviews_corpus_amazon,removePunctuation)
inspect(reviews_corpus_amazon[1:3])          #punctuations are removed

#Remove stopwords :
reviews_corpus_amazon<-tm_map(reviews_corpus_amazon,removeWords,stopwords("english"))  # There are predefined set of stopwords in R
inspect(reviews_corpus_amazon[1:3])          #words like if,but,or are removed

#Remove URL's :
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
reviews_corpus_amazon <- tm_map(reviews_corpus_amazon, content_transformer(removeURL))
inspect(reviews_corpus_amazon[1:3])

#Manage the white spaces :
reviews_corpus_amazon<-tm_map(reviews_corpus_amazon,stripWhitespace)
inspect(reviews_corpus_amazon[1:3])          #spaces are removed

#Now check the data is cleaned properly  :        

inspect(reviews_corpus_amazon[1:5])

#Looks like data is cleaned properly.

#Build a Term Document Matrix :

#In TDM, each row represents a word and each column represents a Document

reviews_amazon_tdm <- TermDocumentMatrix(reviews_corpus_amazon)
str(reviews_amazon_tdm)

#List of 6
#$ i       : int [1:398] 1 2 3 4 5 6 7 8 9 10 ...
#$ j       : int [1:398] 1 1 1 1 1 1 1 1 1 1 ...
#$ v       : num [1:398] 20 10 10 10 10 10 10 10 10 10 ...
#$ nrow    : int 398
#$ ncol    : int 1
#$ dimnames:List of 2
#..$ Terms: chr [1:398] "achieve" "achieved" "advice" "againdid" ...
#..$ Docs : chr "1"
#- attr(*, "class")= chr [1:2] "TermDocumentMatrix" "simple_triplet_matrix"
#- attr(*, "weighting")= chr [1:2] "term frequency" "tf"

class(reviews_amazon_tdm)  #"TermDocumentMatrix"    "simple_triplet_matrix"
reviews_amazon_tdm <- as.matrix(reviews_amazon_tdm)   #"matrix" "array" 
str(reviews_amazon_tdm)
head(reviews_amazon_tdm)

#         Docs
#Terms     1
#achieve  20
#achieved 10
#advice   10
#againdid 10
#againno  10
#againoh  10

#Visual Analysis of Data :

#Bar Plot:

#Take the frequent words used in dataset:

count_amazon <- rowSums(reviews_amazon_tdm)  # provides the no of times a particular word has been used.
head(count_amazon)
#achieve achieved   advice againdid  againno  againoh 
#  20       10       10       10       10       10 

count_amazon <- subset(count_amazon, count_amazon>= 25) # Pull words that were used more than 25 times.
head(count_amazon)
#bit  book books   can  feel first 
#30   260    30    80    40    40 

barplot(count_amazon, las = 2, col = rainbow(50))

#from the barplot, we can figure out that books, mind, subconscious 
#words are used more frequently.


# Word Cloud :


count_amazon_1<- sort(rowSums(reviews_amazon_tdm), decreasing = TRUE) # Sort words in decreasing order.
head(count_amazon_1)

#book         mind subconscious         idea   nnnnnnnnnn          can 
#260          130          120          100           99           80 

set.seed(123)
wordcloud(words = names(count_amazon_1), freq = count_amazon, 
          max.words = 250,random.order = F,
          min.freq =  3 )
# Words like positive, mind, feel just, idea are used frequent.

# We have done the preprocessing of data 
# Not, lets do sentimental Analysis

######################### Sentimental Analysis ###########################

#Load the dataset :

reviews_amazon <- readLines("amazon_Reviews.txt")
str(reviews_amazon)
View(reviews_amazon)
reviews_amazon[1:10]

#Get sentimental Scores :
scores_amazon <- get_nrc_sentiment(reviews_amazon)
head(scores_amazon)
#     anger anticipation disgust fear joy sadness surprise trust negative positive
#1     0            0       0    0   0       0        0     0        0        0
#2     0            0       0    0   0       0        0     0        0        0
#3     0            0       0    0   0       0        0     0        0        0
#4     0            0       0    0   0       0        0     0        0        0
#5     0            0       0    0   0       0        0     0        0        0
#6     0            0       0    0   0       0        0     0        0        0

tail(scores_amazon)

#      anger anticipation disgust fear joy sadness surprise trust negative positive
#1696     0            0       0    0   0       0        0     0        0        0
#1697     0            0       0    0   0       0        0     0        0        0
#1698     0            1       0    1   0       1        0     0        1        3
#1699     0            0       0    0   0       0        0     0        0        0
#1700     0            0       0    0   0       0        0     0        0        0
#1701     0            0       0    0   0       0        0     0        0        0

colSums(scores_amazon)
#anger  anticipation      disgust       fear          joy      sadness 
#140          290           80          170          290           80 
#surprise   trust     negative    positive 
#80          390          180          750 
#From the scores, we can see more points for Trust, positive and Joy

#Bar plot of sentimental scores:

barplot(colSums(scores_amazon), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Amazon Book Reviews')

#Highest scores are for positive words.


# subset


#List of Positive sentences :

positive_items <- which(scores_amazon$positive > 0)
head(reviews_amazon[positive_items])


#List of Negative sentences :

negative_items <- which(scores_amazon$negative > 0)
head(reviews_amazon[negative_items])

##################### Positive and Negative word cloud ################

# Function for Positive and Negative wordcloud :

# Making positive wordcloud function 
makeposwordc = function(x){
   freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
   # matching positive words
   pos.matches = match(names(freq), c(pos.words,"approvals"))
   pos.matches = !is.na(pos.matches)
   freq_pos <- freq[pos.matches]
   names <- names(freq_pos)
   windows()
   wordcloud(names,freq_pos,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

# Making negatiVe wordcloud function
makenegwordc = function(x){	
   freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
   # matching negative words
   neg.matches = match(names(freq), neg.words)
   neg.matches = !is.na(neg.matches)
   freq_neg <- freq[neg.matches]
   names <- names(freq_neg)
   windows()
   wordcloud(names[1:120],freq_neg[1:120],scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

# lOADING +VE AND -VE words  
pos.words=scan(file.choose(), what="character", comment.char=";")	
# read-in positive-words.txt from the assignment
neg.words=scan(file.choose(), what="character", comment.char=";") 	
# read-in negative-words.txt from the assignent

#Call the Positive and Negative functions:

makeposwordc(reviews_amazon_tdm)

#Most frequent used Positive words are miracles, recommended, happiness, like, 
#powerful, success, Trust, Joy,best, inspiring, good and Positive

makenegwordc(reviews_amazon_tdm)

#Now, lets do the deep analysis :

sentimental_amazon <- get_sentences(reviews_amazon)
class(sentimental_amazon)  #Character
str(sentimental_amazon)
head(sentimental_amazon)

sentiment_vector_amazon <- get_sentiment(sentimental_amazon, method = "bing")
head(sentiment_vector_amazon)


afinn_s_v_amazon <- get_sentiment(sentimental_amazon, method = "afinn")
head(afinn_s_v_amazon)

nrc_vector_amazon <- get_sentiment(sentimental_amazon, method="nrc")
head(nrc_vector_amazon)


sum(sentiment_vector_amazon)   #460
mean(sentiment_vector_amazon)  #0.2034
summary(sentiment_vector_amazon)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1.0000  0.0000  0.0000  0.2034  0.0000  4.0000

# Data Vizualization :

plot(sentiment_vector_amazon, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")


# Extract the most negative sentence

negative <- sentimental_amazon[which.min(sentiment_vector_amazon)]
negative
#"But this book is a waste of money."

# Extract the most positive sentence

positive <- sentimental_amazon[which.max(sentiment_vector_amazon)]
positive
#"And he presents simple, practical, and proven-effective exercises that can 
#turn your mind into a powerful tool for improving your everyday life."

# more depth
poa_v_amazon<-reviews_amazon
poa_sent_amazon <- get_sentiment(poa_v_amazon, method="bing")
plot(
  poa_sent_amazon, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# percentage based figures
percent_vals_amazon <- get_percentage_values(poa_sent_amazon)
percent_vals_amazon

plot(
  percent_vals_amazon, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values_amazon <- get_transformed_values(
  poa_sent_amazon, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values_amazon, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# Result : We have more positives reviews for Amazon Book.

