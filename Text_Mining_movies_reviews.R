#1) Extract movie reviews for any movie from IMDB and perform sentimental analysis

#Load the required packages:

install.packages("rvest")
library(rvest)
install.packages("XML")
library(XML)
install.packages("magrittr")
library(magrittr)
library(tm)
library(wordcloud)
#library(wordcloud2)
install.packages("syuzhet")
library(syuzhet)
install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
#library(dplyr)


# Extract the reviews of movie Dil Bechara from the website and save it in a variable:

link <- "https://www.imdb.com/title/tt8110330/reviews?ref_=tt_ov_rt"
IMDB_reviews_DB <- NULL

for (i in 1:10){
  murl <- read_html(as.character(paste(link,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews_DB <- c(IMDB_reviews_DB,rev)
}
length(IMDB_reviews_DB)   #730


#Write the movie reviews in table and save it in Local repository:
getwd()
#setwd("C:/Users/admin/Desktop/python program files")
write.table(IMDB_reviews_DB,"Dil_Bechara_Reviews.txt",row.names = F)

#Read the file:
reviews_DB <- read.delim('Dil_Bechara_Reviews.txt')
View(reviews_DB)
str(reviews_DB)    #490 obs. of  1 variable
class(reviews_DB)   #data.frame

#Lets understand the data :

head(reviews_DB)
#1 One of the finest acting I've ever seen.I suggest you to please watch this. 
#I think this is far better than movies originated from nepotism. Shushant Sir 
#has done great work and lived the moment in this film. Everything was perfect.
#2\n                
#3 This is the only Indian movie which made me cry..... You need to be 
#emotionally strong specially after the dismissal of the Sushant Singh Rajput.. 
#he is great and he was capable to be no 1 in india.. Because of nepotism we 
#lost a well deserved star. I was deeply upset by the ending but overall it was 
#a great movie
#4\n                
#5 Dear Manny/Augustas/Sushant Singh Rajput\nyou did leave a mark, you left all 
#your viewers speechless.\nBe it in the reel world or the real world.\nYou were 
#the most amazing character and I enjoyed you act, and sobbed at the same time, 
#watching you smile and crack jokes made me so emotional.\nHow could a person 
#this cheerful with the brightest not with us anymore.\nThis movie was very warm 
#just the right mix of humor and drama and emotions.\nIts so fresh that one can 
#re-watch it a 100 times and still wont get bored.\nI recommend it to all the 
#people out their, because it is not just about cancer or just the regular 
#teenage love story, it far more than that.\nGive it a watch and I'm sure you'll 
#agree with me.
#6\n 


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

reviews_corpus_DB <-Corpus(VectorSource(reviews_DB))
print(reviews_corpus_DB)
#Metadata:  corpus specific: 1, document level (indexed): 0
#Content:  documents: 1
class(reviews_corpus_DB)   

#Inspect top five corpus data :
inspect(reviews_corpus_DB[1:5])

#Convert into lower case :

reviews_corpus_DB<-tm_map(reviews_corpus_DB,tolower)
inspect(reviews_corpus_DB[1:3])         #Changed to lower case

#Remove the numbers :
reviews_corpus_DB<-tm_map(reviews_corpus_DB,removeNumbers)
inspect(reviews_corpus_DB[1:5])          #Numbers are removed

#Remove Punctuations :
reviews_corpus_DB<-tm_map(reviews_corpus_DB,removePunctuation)
inspect(reviews_corpus_DB[1:3])          #punctuations are removed

#Remove stopwords :
reviews_corpus_DB<-tm_map(reviews_corpus_DB,removeWords,stopwords())  # There are predefined set of stopwords in R
inspect(reviews_corpus_DB[1:3])          #words like if,but,or are removed

#Remove URL's :
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
reviews_corpus_DB <- tm_map(reviews_corpus_DB, content_transformer(removeURL))
inspect(reviews_corpus_DB[1:3])

#Remove unnecessary words also :
reviews_corpus_DB<-tm_map(reviews_corpus_DB,removeWords, c('can','film'))
reviews_corpus_DB<-tm_map(reviews_corpus_DB,removeWords, c('movie','movies'))
reviews_corpus_DB <- tm_map(reviews_corpus_DB, gsub,pattern = 'character', replacement = 'characters')
inspect(reviews_corpus_DB[1:3])

#Manage the white spaces :
reviews_corpus_DB<-tm_map(reviews_corpus_DB,stripWhitespace)
inspect(reviews_corpus_DB[1:3])          #spaces are removed

#Now check the data is cleaned properly  :        

inspect(reviews_corpus_DB[1:5])

#Looks like data is cleaned properly.

#Build a Term Document Matrix :

#In TDM, each row represents a word and each column represents a Document

reviews_DB_tdm <- TermDocumentMatrix(reviews_corpus_DB)
str(reviews_DB_tdm)
#List of 6
#$ i       : int [1:357] 1 2 3 4 5 6 7 8 9 10 ...
#$ j       : int [1:357] 1 1 1 1 1 1 1 1 1 1 ...
#$ v       : num [1:357] 10 10 10 20 10 20 10 40 10 10 ...
#$ nrow    : int 357
#$ ncol    : int 1
#$ dimnames:List of 2
#..$ Terms: chr [1:357] "100" "1010" "2020" "absolutely" ...
#..$ Docs : chr "1"
#- attr(*, "class")= chr [1:2] "TermDocumentMatrix" "simple_triplet_matrix"
#- attr(*, "weighting")= chr [1:2] "term frequency" "tf"

class(reviews_DB_tdm)  #"TermDocumentMatrix"    "simple_triplet_matrix"
reviews_DB_tdm <- as.matrix(reviews_DB_tdm)   #"matrix" "array" 
str(reviews_DB_tdm)
head(reviews_DB_tdm)

#           Docs
#Terms       1
#absolutely 20
#act        10
#acting     20
#actingthe  10
#actor      40
#actress    10

#Visual Analysis of Data :

#Bar Plot:

#Take the frequent words used in dataset:

count_DB <- rowSums(reviews_DB_tdm)  # provides the no of times a particular word has been used.
head(count_DB)
#absolutely        act     acting     actingthe      actor    actress 
#    20             10         20         10         40         10 

count_DB <- subset(count_DB, count_DB>= 25) # Pull words that were used more than 25 times.
head(count_DB)
#actor  adaptation     amazing        best      better characterss 
#40          30          60          40          30          30

barplot(count_DB, las = 2, col = rainbow(50))

#from the barplot, we can figure out that Sushant, will, just, lifr, live and amazing
#words are used more frequently.
#We can say that movie Dil Bechara got most of the positive reviews.

# Word Cloud :


count_movies_DB<- sort(rowSums(reviews_DB_tdm), decreasing = TRUE) # Sort words in decreasing order.
head(count_movies_DB)

#sushant    will     one    life    live   watch 
#   140     110      89      80      80      70


set.seed(123)
wordcloud(words = names(count_movies_DB), freq = count_movies, 
          max.words = 250,random.order = F,
          min.freq =  3 )
# Words like Sushant,Just amazing, Watch, just are used frequent.

#Many People watched the movie.

# We have done the preprocessing of data 
# Not, lets do sentimental Analysis

######################### Sentimental Analysis ###########################

#Load the dataset :

reviews_DB <- readLines("Dil_Bechara_Reviews.txt")
View(reviews_DB)
reviews_DB[1:5]

#Get sentimental Scores :
scores_DB <- get_nrc_sentiment(reviews_DB)
head(scores_DB)

#   anger anticipation disgust fear joy sadness surprise trust negative positive
#1     0            0       0    0   0       0        0     0        0        0
#2     1            2       1    1   1       1        0     3        1        2
#3     0            0       0    0   0       0        0     0        0        0
#4     0            0       0    0   0       0        0     0        0        0
#5     0            0       0    0   0       0        0     0        0        0
#6     3            1       2    1   1       5        1     1        5        2

#From the above table, we can see the sentimental scores of all the words along with
#Positive and Negative scores also.

colSums(scores_DB)
#anger anticipation      disgust         fear          joy      sadness 
#120          380           80          240           290          260 
#surprise        trust     negative     positive 
#120            330          300          510 

#From the scores, we can see more points for Joy,trust and positive

#Bar plot of sentimental scores:

barplot(colSums(scores_DB), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Dil Bechara Reviews')

#Highest scores are for positive words.


# subset


#List of Positive sentences :

positive_items <- which(scores_DB$positive > 0)
head(reviews_DB[positive_items])


#List of Negative sentences :

negative_items <- which(scores_DB$negative > 0)
head(reviews_DB[negative_items])

##################### Positive and Negative words: word cloud ################

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

makeposwordc(reviews_DB_tdm)

#Most frequent used Positive words are miracles, recommended, happiness, like, 
#powerful, success, Trust, Joy,best, inspiring, good and Positive

makenegwordc(reviews_DB_tdm)


#Now, lets do the deep analysis :

sentimental_DB <- get_sentences(reviews_DB)
class(sentimental_DB)  #Character
str(sentimental_DB)
head(sentimental_DB)

#[1] "\"x\""                                                              
#[2] "\"One of the finest acting I've ever seen."                         
#[3] "I suggest you to please watch this."                                
#[4] "I think this is far better than movies originated from nepotism."   
#[5] "Shushant Sir has done great work and lived the moment in this film."
#[6] "Everything was perfect.\"" 


sentiment_vector_DB <- get_sentiment(sentimental_DB, method = "bing")
head(sentiment_vector_DB)
#0 1 0 0 2 1

afinn_s_v_DB <- get_sentiment(sentimental_DB, method = "afinn")
head(afinn_s_v_DB)
#0 0 1 2 3 3

nrc_vector_DB <- get_sentiment(sentimental_DB, method="nrc")
head(nrc_vector_DB)
#0  0  0 -1  1  1

sum(sentiment_vector_DB)   #400
mean(sentiment_vector_DB)  #0.224
summary(sentiment_vector_DB)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-2.0000  0.0000  0.0000  0.2246  1.0000  3.0000


# Data Vizualization :

plot(sentiment_vector_DB, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")


# Extract the most negative sentence

negative <- sentimental_DB[which.min(sentiment_vector_DB)]
negative
#"This movie will tell you what lies between birth and death is life that we 
#generally forget to live..."

# Extract the most positive sentence

positive <- sentimental_DB[which.max(sentiment_vector_DB)]
positive
#"This movie was very warm just the right mix of humor and drama and emotions."

# more depth
poa_v_DB<-reviews_DB
poa_sent_DB <- get_sentiment(poa_v_DB, method="bing")
plot(
  poa_sent_DB, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# percentage based figures
percent_vals_DB <- get_percentage_values(poa_sent_DB)
percent_vals_DB

plot(
  percent_vals_DB, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values_DB <- get_transformed_values(
  poa_sent_DB, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values_DB, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# Result : We have more positives reviews for movie Dil Bechara




#2) Extract anything you choose from the internet and do some research on how we extract using R
#Programming and perform sentimental analysis.

#For this : I am going to extract the reviews of movie : Aquaman from IMDb 


# Extract the reviews of movie Aquaman from the website and save it in a variable:

link <- "https://www.imdb.com/title/tt1477834/reviews?ref_=tt_ov_rt"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(link,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)   #730

#Write the reviews in table and save it in Local repository:
getwd()
setwd("C:/Users/admin/Desktop/python program files")
write.table(IMDB_reviews,"Reviews.txt",row.names = F)

#Read the file:
reviews <- read.delim('Reviews.txt')
View(reviews)
str(reviews)    #490 obs. of  1 variable
class(reviews)

#Lets understand the data :

head(reviews)
#1 And it has nothing to do with the comic books.Bad and pointlessWhat is the point 
  #of this film? A big beefcake? Because that's literally the only storyline this mess
  #has going for it. Pointless characters, horrible CGI for this modern time, 
  #bad acting, I could continue. If I had went to the theater to see this, I would be 
  #trying to get my money back. I gave it a 3 only because of the nostalgia I have for 
  #Aquaman. That's all that is good about this excuse of an action/family film.
#2\n                
#3 What's with the 10 out of 10 reviews? really?. Sure it's probably the best 
   #(or second best) DC Justice league era, but that's not difficult.\nLike all the 
   #other offerings, it's overblown, wooden, one dimensional and over reliant on over 
   #the top CGI.\nAs a child I was always a DC fan and I want them more than anything to
   #bring this thing together, but Marvel have certainly laid down a tough marker for 
   #how it should be and DC still haven't met that challenge.\nThe main thing missing 
  #from the film is charm and simplicity, sure Momoa just about saves the day and I 
  #think he can develop Aquaman into a loved character, and the film is a step in the 
  #right direction. But 10 out of 10 it most certainly isn't.
#4\n                
#5 If you are a preteen, the story may work for you. If you have any common sense, 
   #the bad dialogue and blatant rip off of every sci-fi and fantasy movie cliche 
   #will drive you crazy.Special effects are decent, and the story moves along at a 
   #decent pace. Momoa carries the film despite being surrounded by one dimensional 
   #characters with digitally botoxed faces. I would never watch this movie again.
#6\n

#tail(reviews)

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

reviews_corpus <-Corpus(VectorSource(reviews))
print(reviews_corpus)
#Metadata:  corpus specific: 1, document level (indexed): 0
#Content:  documents: 1
class(reviews_corpus)   

#Inspect top five corpus data :
inspect(reviews_corpus[1:5])

#Convert into lower case :

reviews_corpus<-tm_map(reviews_corpus,tolower)
inspect(reviews_corpus[1:3])         #Changed to lower case

#Remove the numbers :
reviews_corpus<-tm_map(reviews_corpus,removeNumbers)
inspect(reviews_corpus[1:3])          #Numbers are removed

#Remove Punctuations :
reviews_corpus<-tm_map(reviews_corpus,removePunctuation)
inspect(reviews_corpus[1:3])          #punctuations are removed

#Remove stopwords :
reviews_corpus<-tm_map(reviews_corpus,removeWords,stopwords())  # There are predefined set of stopwords in R
inspect(reviews_corpus[1:3])          #words like if,but,or are removed

#Manage the white spaces :
reviews_corpus<-tm_map(reviews_corpus,stripWhitespace)
inspect(reviews_corpus[1:3])          #spaces are removed

#Remove URL's :
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
reviews_corpus <- tm_map(reviews_corpus, content_transformer(removeURL))
inspect(reviews_corpus[1:3])

#Remove unnecessary words also :
reviews_corpus<-tm_map(reviews_corpus,removeWords, c('can','film'))
reviews_corpus<-tm_map(reviews_corpus,removeWords, c('movie','movies'))
reviews_corpus <- tm_map(reviews_corpus, gsub,pattern = 'character', replacement = 'characters')
inspect(reviews_corpus[1:3])

#Manage the white spaces :
reviews_corpus<-tm_map(reviews_corpus,stripWhitespace)
inspect(reviews_corpus[1:3])          #spaces are removed


#Now check the data is cleaned properly  :        

inspect(reviews_corpus[1:5])

#Looks like data is cleaned properly.


#Build a Term Document Matrix :

#In TDM, each row represents a word and each column represents a Document

reviews_tdm <- TermDocumentMatrix(reviews_corpus)
str(reviews_tdm)

#List of 6
#$ i       : int [1:822] 1 2 3 4 5 6 7 8 9 10 ...
#$ j       : int [1:822] 1 1 1 1 1 1 1 1 1 1 ...
#$ v       : num [1:822] 10 10 10 10 20 20 20 10 20 10 ...
#$ nrow    : int 822
#$ ncol    : int 1
#$ dimnames:List of 2
#..$ Terms: chr [1:822] "absolutely" "accomplishment" "achieve" "across" ...
#..$ Docs : chr "1"
#- attr(*, "class")= chr [1:2] "TermDocumentMatrix" "simple_triplet_matrix"
#- attr(*, "weighting")= chr [1:2] "term frequency" "tf"

class(reviews_tdm)  #"TermDocumentMatrix"    "simple_triplet_matrix"
reviews_tdm <- as.matrix(reviews_tdm)   #"matrix" "array" 
str(reviews_tdm)
head(reviews_tdm)

              #Docs
#Terms           1
#absolutely     10
#accomplishment 10
#achieve        10
#across         10
#act            20
#acting         20


#Visual Analysis of Data :

#Bar Plot:

#Take the frequent words used in dataset:

count <- rowSums(reviews_tdm)  # provides the no of times a particular word has been used.
head(count)
#absolutely accomplishment      achieve         across          act 
#   10             10             10             10             20 
#acting 
#20 

count <- subset(count, count>= 25) # Pull words that were used more than 25 times.
head(count)
#actually   also  another   anyone  aquaman      bad 
#    30       40       30       30      200       50 
barplot(count, las = 2, col = rainbow(50))

#from the barplot, we can figure out that aquaman,likes, good, just etc words are used 
#more frequently.
#We can say that movie Aquaman got most of the reviews.

# Word Cloud :


count_movies<- sort(rowSums(reviews_tdm), decreasing = TRUE) # Sort words in decreasing order.
head(count_movies)

#aquaman    like        just        good      really     characterss 
#200         200         150         120          90          80

set.seed(123)
wordcloud(words = names(count_movies), freq = count_movies, 
          max.words = 250,random.order = F,
          min.freq =  3 )
# Words like aquaman, like, good, just, characterless are used frequent.

# We have done the preprocessing of data 
# Not, lets do sentimental Analysis

######################### Sentimental Analysis ###########################

#Load the Dataset :

IMDB_Reviews <- readLines("Reviews.txt")
class(IMDB_Reviews)
IMDB_Reviews[1:5]

#Get sentimental Scores :
scores <- get_nrc_sentiment(IMDB_Reviews)
head(scores)
#    anger anticipation disgust fear joy sadness surprise trust negative positive
#1     0        0          0    0     0       0        0     0        0        0
#2     3        4          3    2     2       2        2     3        5        4
#3     0        0          0    0     0       0        0     0        0        0
#4     0        0          0    0     0       0        0     0        0        0
#5     0        0          0    0     0       0        0     0        0        0
#6     0        0          0    1     0       0        0     1        0        2

#From the above table, we can see the sentimental scores of all the words along with
#Positive and Negative scores also.

#For ex: on 2nd review, 
#disgust,trust : 3 scores
#fear,joy,sadness,surprise : 2 scores 
#Negative : 5
#positive : 4

#Bar plot of sentimental scores:

barplot(colSums(scores), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews')
#Highest scores are for positive words


# subset


#List of Positive sentences :

positive_items <- which(scores$positive > 0)
head(IMDB_Reviews[positive_items])


#List of Negative sentences :

negative_items <- which(scores$negative > 0)
head(IMDB_Reviews[negative_items])


##################### Positive and Negative words: word cloud ################

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

makeposwordc(reviews_tdm)

makenegwordc(reviews_tdm)


#Now, lets do the deep analysis   :

sentimental_sent <- get_sentences(IMDB_Reviews)
class(sentimental_sent)  #Character
str(sentimental_sent)
head(sentimental_sent)

sentiment_vector <- get_sentiment(sentimental_sent, method = "bing")
head(sentiment_vector)

afinn_s_v <- get_sentiment(sentimental_sent, method = "afinn")
head(afinn_s_v)

nrc_vector <- get_sentiment(sentimental_sent, method="nrc")
head(nrc_vector)

sum(sentiment_vector)   #260
mean(sentiment_vector)  #0.1139
summary(sentiment_vector)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-6.000   0.000   0.000   0.114   1.000   8.000


# Data Vizualization :

plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")


# TExtract the most negative sentence

negative <- sentimental_sent[which.min(sentiment_vector)]
negative

#"But the main issue, which makes this movie unwatchable without at least 4 cans 
#of beer, is the fact that its linear as hell, there are 0 unpredictable plot twists,
#from the beginning of the movie (or even from trailers) you for sure know how its 
#going to end.

# Extract the most positive sentence
positive <- sentimental_sent[which.max(sentiment_vector)]
positive
#"His biggest accomplishment, however - the film's greatest strength - 
#is something that only few blockbusters ever achieve: he successfully combines 
#a genuine sense of wonder and awe with complete, unabashed fun.So, to sum up this 
#review: against my expectations, the superhero film I least expected to like managed
#to do what not a single superhero movie so far had done for me; it filled me with 
#an urge to immediately see it again, because I was so in love with its bonkers,

# more depth
poa_v<-IMDB_Reviews
poa_sent <- get_sentiment(poa_v, method="bing")
plot(
  poa_sent, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# percentage based figures
percent_vals <- get_percentage_values(poa_sent)
percent_vals

plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values <- get_transformed_values(
  poa_sent, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# Result : We have more positives reviews for movie Aquaman



