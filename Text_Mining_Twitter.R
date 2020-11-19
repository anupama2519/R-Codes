
#Text Mining

#1) Extract tweets for any user (try choosing a user who has more tweets) and
#Perform sentimental analysis on the tweets extracted from the above

#Load the required packages :

install.packages("devtools")
devtools::install_github("lchiffon/wordcloud2")
devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
devtools::install_version("httr",version="0.6.0",repos="http://cran.us.r-project.org")
install.packages("twitteR", repos = "http://cran.us.r-project.org")
install.packages("RCurl", repos = "http://cran.us.r-project.org")
install.packages("httr", repos = "http://cran.us.r-project.org")
install.packages("syuzhet", repos = "http://cran.us.r-project.org")
install.packages("ROAuth")
install.packages("base64enc")
install.packages("httpuv")
install.packages("rlang")
library(httpuv)
library("ROAuth")
library(twitteR)
library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)
library(base64enc)
library(rlang)
##################################Extract Narendra Modi tweet###################################### 

# Login Credentials :

#Consumer Key (API Key)
#Consumer Secret (API Secret)
#Access Token
#Access Token Secret
#These keys and tokens will be used to extract data from Twitter in R.

login_cred <- OAuthFactory$new(consumerKey='ZIxPIvj21hhddkMxdjgt3wf94', # Consumer Key (API Key)
                         consumerSecret='BAxTBGvVKZNPiCRADNTNK3VrLO3wrK6OUms3bJXLSpMZ8z5IDI', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(login_cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

setup_twitter_oauth("ZIxPIvj21hhddkMxdjgt3wf94", # Consumer Key (API Key)
                    "BAxTBGvVKZNPiCRADNTNK3VrLO3wrK6OUms3bJXLSpMZ8z5IDI", #Consumer Secret (API Secret)
                    "396521281-5OZ7cS7Ib90Akgdcx4ysPvZ2hQ8uFzGzXuxfxKHB",  # Access Token
                    "MDbqjOaaoMou3vfjSgEiev6omUkkatz692mQXzH8mVP3V")  #Access Token Secret

# Search for Narendra Modi Tweets :

tweet_NM = searchTwitter("@narendramodi", n = 100, lang = "en")
tweet_NM_DF = twListToDF(tweet_NM)
dim(tweet_NM_DF)    #100   16
View(tweet_NM_DF)
colnames(tweet_NM_DF)
#[1] "text"          "favorited"     "favoriteCount" "replyToSN"     "created"      
#[6] "truncated"     "replyToSID"    "id"            "replyToUID"    "statusSource" 
#[11] "screenName"    "retweetCount"  "isRetweet"     "retweeted"     "longitude"    
#[16] "latitude"

#Write the file in Local Repository :

getwd()
setwd("C:/Users/admin/Desktop/python program files")
write.csv(tweet_NM_DF, "Tweets_NM_new.csv",row.names = F)

#Read the file:
tweet_NM <- read.csv('Tweets_NM_new.csv')


#Lets understand the data :

head(tweet_NM$text)
#1] "RT @Sagar_12_1: Why Sena attacks Arnab Goswami Why ????\n\nOur Constitution give rights to ask questions...\n\nShame on System !\n\nShame on Maha."
#[2] "RT @zoho: The results are out! \n\nWe've won the Digital India #AatmaNirbharApp Innovation Challenge for the \"Business\" and \"Office\" categori."
#[3] "RT @SaoSweta: #NTAPOSTPONEJEE_NEET\n#NTAPOSTPONEJEE_NEET\n#NTAPOSTPONEJEE_NEET\nRESPECTED SIR\n@DrRPNishank\nPlease take ur decision soon\nIts ma."
#[4] "RT @Karan_sngh034: @varunvagish was stuck in Kyrgyzstan bcz of registration issue he wnt to our @IndiaInKyrgyz embassy seeking help whr ins."      
#[5] "RT @ShwetaR78461653: Honorable Prime Minister, we are with your dreams of a self-reliant India, but is it possible without the old pension."       
#[6] "RT @narendramodi: Interacting with @BJP4AnN Karyakartas. https://t.co/QpTyUSHYHt"                                

#This contains a lot of URLs, hashtags and other twitter handles. 
#We will remove all these using the gsub function.

#Clean the Tweets :


tweet_NM$text <- gsub("http.*","",tweet_NM$text)
tweet_NM$text <- gsub("https.*","",tweet_NM$text)
tweet_NM$text <- gsub("#.*","",tweet_NM$text)
tweet_NM$text <- gsub("@.*","",tweet_NM$text)
tweet_NM$text =gsub("&amp", "", tweet_NM$text)
tweet_NM$text = gsub("&amp", "", tweet_NM$text)
tweet_NM$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet_NM$text)
tweet_NM$text = gsub("@\\w+", "", tweet_NM$text)
tweet_NM$text = gsub("[[:punct:]]", "", tweet_NM$text)
tweet_NM$text = gsub("[[:digit:]]", "", tweet_NM$text)
tweet_NM$text = gsub("http\\w+", "", tweet_NM$text)
tweet_NM$text = gsub("[ \t]{2,}", "", tweet_NM$text)
tweet_NM$text = gsub("^\\s+|\\s+$", "", tweet_NM$text)
tweet_NM$text = gsub("\n", "", tweet_NM$text)



#Now check the data is cleaned properly  :        

head(tweet_NM$text)

#[1] "Why Sena attacks Arnab Goswami Why Our Constitution give rights to 
#ask questionsShame on System Shame on Maha."            
#[2] "The results are out We've won the Digital India AatmaNirbharApp 
#Innovation Challenge for the Business and Office categori."
#[3] "NTAPOSTPONEJEENEETNTAPOSTPONEJEENEETNTAPOSTPONEJEENEETRESPECTED 
#SIRPlease take ur decision soonIts ma."                    
#[4] "was stuck in Kyrgyzstan bcz of registration issue he wnt to ourembassy 
#seeking help whr ins."                              
#[5] "Honorable Prime Minister we are with your dreams of a selfreliant 
#India but is it possible without the old pension."       
#[6] "Interacting withKaryakartas"  

#Looks like data is cleaned properly.

#Visual Analysis of Data :

wordcloud(tweet_NM$text,min.freq = 30,random.order = FALSE)

#From the plot, we can say that the words India, Nicobar,andaman etc... 
#are used frequently in data.

######################### Sentimental Analysis ###########################

#Get sentimental Scores :

NM_tweet <- as.vector(tweet_NM)

attach(NM_tweet)

scores_NM <- get_nrc_sentiment(NM_tweet$text)
head(scores_NM)

#    anger anticipation disgust fear joy sadness surprise trust negative positive
#1     0            0       1    1   0       1        0     1        1        0
#2     1            0       0    1   0       0        0     0        1        1
#3     0            0       0    0   0       0        0     0        0        0
#4     0            0       0    0   0       0        0     0        0        0
#5     0            0       0    0   0       0        0     1        0        2
#6     0            0       0    0   0       0        0     0        0        0

#From the above table, we can see the different emotions scores of each tweets.

colSums(scores_NM)

#anger  anticipation      disgust         fear          joy      sadness 
#  13           16            6           29           17           22 
#surprise     trust     negative     positive 
#  6           41           38           65 

#From the scores, we can see more points for Joy,trust and positive

#Bar plot of sentimental scores:

barplot(colSums(scores_NM), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Dil Bechara Reviews')

#Highest scores are for positive words.

# subset

#List of Positive sentences :

positive_items_NM <- which(scores_NM$positive > 0)
head(text[positive_items_NM])


#List of Negative sentences :

negative_items_NM <- which(scores_NM$negative > 0)
head(text[negative_items_NM])


#Now, lets do the deep analysis :

sentimental_NM <- get_sentences(NM_tweet$text)
class(sentimental_NM)  #Character
str(sentimental_NM)
head(sentimental_NM)

#[1] "Why Sena attacks Arnab Goswami Why Our Constitution give rights to ask questionsShame on System Shame on Maha."            
#[2] "The results are out We've won the Digital India AatmaNirbharApp Innovation Challenge for the Business and Office categori."
#[3] "NTAPOSTPONEJEENEETNTAPOSTPONEJEENEETNTAPOSTPONEJEENEETRESPECTED SIRPlease take ur decision soonIts ma."                    
#[4] "was stuck in Kyrgyzstan bcz of registration issue he wnt to ourembassy seeking help whr ins."                              
#[5] "Honorable Prime Minister we are with your dreams of a selfreliant India but is it possible without the old pension."       
#[6] "Interacting withKaryakartas"  

sentiment_vector_NM <- get_sentiment(sentimental_NM, method = "bing")
head(sentiment_vector_NM)
#-2  2  0 -2  1  0

afinn_s_v_NM <- get_sentiment(sentimental_NM, method = "afinn")
head(afinn_s_v_NM)
#-3  3  0  0  1  0

nrc_vector_NM <- get_sentiment(sentimental_NM, method="nrc")
head(nrc_vector_NM)
#-1  0  0  0  2  0

sum(sentiment_vector_NM)   #10
mean(sentiment_vector_NM)  #0.1
summary(sentiment_vector_NM)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-2.0     0.0     0.0     0.1     0.0     4.0 

# Data Vizualization :

plot(sentiment_vector_NM, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")


# Extract the most negative sentence

negative <- sentimental_NM[which.min(sentiment_vector_NM)]
negative
#[1] "Why Sena attacks Arnab Goswami Why Our Constitution give rights to ask questionsShame on System Shame on Maha."

# Extract the most positive sentence

positive <- sentimental_NM[which.max(sentiment_vector_NM)]
positive
#[1] "Great Super Bold Speech by Brave and Strong INC SupporterTitle \"Rozgar doji\"RozgarDo UUBUCUCUUEUUUB M."

# more depth
poa_v_NM<-tweet_NM$text
poa_sent_NM <- get_sentiment(poa_v_NM, method="bing")
plot(
  poa_sent_NM, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

ft_values_NM <- get_transformed_values(
  poa_sent_NM, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values_NM, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

#Result : Positive emotions are more in the Narendra Modi tweets.