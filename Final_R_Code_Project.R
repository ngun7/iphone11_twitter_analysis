#***************      MSIS 5600 PROJECT    ***************** 

#R code for customer feedback analysis of Apple products

#Importing necessary packages for the code down the path

library(twitteR)
library(rtweet)
library(stringr)
library(rvest)
library(xml2)
library(dplyr)
library(tidyverse)
library(tidytext)
library(xlsx)
library(textclean)
library(qdapRegex)
library(tm)
library(SnowballC)
library(scales)
library(ggplot2)
library(wordcloud)
library(udpipe)
library(lattice)
library(wordcloud2)
library(lattice)
library(NLP)
library(textdata)
library(rlang)
library(RColorBrewer)
library(reshape2)

#Setting working directory
setwd("C:\\Users\\hp\\Desktop\\MSIS 5600\\Twitter_data_project")


#authenticating token details to access twitter data
twitter_token <- create_token(
  app = 'MSIS 5600_Assignment',
  consumer_key = "xxxxx",
  consumer_secret = "xxx",
  access_token = "xxxxx",
  access_secret = "xxx")


################   tweets TEXT DATA ONLY as of date 10/15        ################
################   Data extraction and cleaning for 10/15 data   ################


#First data file name: Twitter_data_10_15
#Scraped tweets as of date 10/15/2019 

rt <- search_tweets("#iphone11", 
                    n = 1500, 
                    include_rts = FALSE, 
                    lang = "en", 
                    geocode = lookup_coords("usa"),
                    token = twitter_token)

#exporting tweets in Dataset "Twitter_data_10_15" to excel sheet

#write.csv(rt$text, file="C:\\Users\\hp\\Desktop\\MSIS 5600\\Twitter_data_10_15.txt", sep = '\t')
write.csv(rt$text, file="Twitter_data_10_15.csv", sep = ',')


#Second data file name: Twitter_data_10_15_v2
#Scraped second set of tweets with more key words as of date 10/15/2019 
data_v2 <- search_tweets("(iphone11 OR Apple OR iphone11 OR iphone11pro OR iphone11promax OR slofies OR #iphone OR #iphone11)", 
                         n = 1500, 
                         include_rts = FALSE, 
                         lang = "en", 
                         token = twitter_token)
#data_v2$text

#exporting tweets in dataset "Twitter_data_10_15_v2" to excel sheet
write.csv(data_v2$text, file="Twitter_data_10_15_v2.csv", sep = ',')



#Third data file name: Twitter_data_10_15_v3.csv
#Scraped third set of tweets as of date 10/15/2019 
data_3 <- search_tweets("(iphone11 OR iphone11pro OR iphone11promax #iphone OR #iphone11)", 
                        n = 5000, 
                        include_rts = FALSE, 
                        lang = "en", 
                        token = twitter_token)
#data_v3$text

#exporting tweets in datset "Twitter_data_10_15_v3.csv" to excel file
write.csv(data_3$text, file="Twitter_data_10_15_v3.csv", sep = ',')

#***** Above is the code for twitter data downlaoded* *******

#Data Preparation -------------

#Attaching above datasets
#reading first 3 datasets
df1 <- read.csv("Twitter_data_10_15.CSV",header = TRUE, sep = "," ,stringsAsFactors = FALSE)
df2 <- read.csv("Twitter_data_10_15_v2.CSV",header = TRUE, sep = "," ,stringsAsFactors = FALSE)
df3 <- read.csv("Twitter_data_10_15_v3.CSV",header = TRUE, sep = "," ,stringsAsFactors = FALSE)

#Appending the above mentioned datasets
df_text_dataset <- rbind(df1,df2,df3)
names(df_text_dataset) <- c("row","text")

#taking only unique tweets from the data and loading it to a file.
unique_tweets <- unique(df_text_dataset$text)
tweet <- data.frame(unique_tweets, stringsAsFactors = FALSE)
#write.csv(tweet,"Unique_tweets.csv", row.names = FALSE)
names(tweet) <- c("text")

#Data Cleaning ---------

#removing the URL from the data
tweet$text <- rm_url(tweet$text, pattern=pastex("@rm_twitter_url", "@rm_url"))

#cleaning the data
corpustweet = Corpus(VectorSource(tweet$text))

#converting to lower case
corpustweet = tm_map(corpustweet, tolower)

#removing punctuation marks
corpustweet = tm_map(corpustweet, removePunctuation)

#removing stop words
corpustweet = tm_map(corpustweet, removeWords, c(stopwords("english")))

#corpuscloud = tm_map(corpustweet, removeWords, c(stopwords("english"),"iphone11","appl","iphon","iphone","apple","iphone11pro","iphone11promax","get","amp","now","dont","good","buy",
#                                                "take","need","keep","nba","must","can", "phone", "just", "lastipados",
#                                                "device"))

#Creating WordCloud
#wordcloud(corpuscloud,colors=rainbow(7),max.words=150)

#removing whitespaces
corpustweet = tm_map(corpustweet, stripWhitespace)


#Data Implementation -----------


#stemming the words
corpustweet = tm_map(corpustweet, stemDocument)


#frequency_of_tweets
freq_tweet = DocumentTermMatrix(corpustweet)
freq_tweet

tweet_Sparse = removeSparseTerms(freq_tweet, 0.995)

#convert to a Data Frame
#Exporting document term matrix of tweets into excel file
tweet_Sparse = as.data.frame(as.matrix(tweet_Sparse))
str(tweet_Sparse)
write.csv(tweet_Sparse,"tweet_Sparse.csv",row.names = FALSE)



################   tweets - complete data(required attributes) for specified dates ################
################   Data extraction and cleaning tweets of specified dates   ################



#1st - dataset
#Scraping tweets as of date 10/24/2019

data_v3 <- search_tweets("(iphone11 OR iphone11pro OR iphone11promax #iphone OR #iphone11)", 
                         n = 5000, 
                         include_rts = FALSE, 
                         lang = "en", 
                         token = twitter_token)

#start - converting list data to character data
data_v3$urls_url  <- vapply(data_v3$urls_url, paste, collapse = ", ", character(1L))
data_v3$media_url <- vapply(data_v3$media_url, paste, collapse = ", ", character(1L))
data_v3$media_type <- vapply(data_v3$media_type, paste, collapse = ", ", character(1L))
data_v3$ext_media_expanded_url <- vapply(data_v3$ext_media_expanded_url, paste, collapse = ", ", character(1L))
data_v3$mentions_screen_name <- vapply(data_v3$mentions_screen_name, paste, collapse = ", ", character(1L))
data_v3$coords_coords <- vapply(data_v3$coords_coords, paste, collapse = ", ", character(1L))
data_v3$hashtags <- vapply(data_v3$hashtags, paste, collapse = ", ", character(1L))
data_v3$urls_t.co <- vapply(data_v3$urls_t.co, paste, collapse = ", ", character(1L))
data_v3$media_t.co <- vapply(data_v3$media_t.co, paste, collapse = ", ", character(1L))
data_v3$ext_media_url <- vapply(data_v3$ext_media_url, paste, collapse = ", ", character(1L))
data_v3$bbox_coords <- vapply(data_v3$bbox_coords, paste, collapse = ", ", character(1L))
data_v3$symbols <- vapply(data_v3$symbols, paste, collapse = ", ", character(1L))
data_v3$urls_expanded_url <- vapply(data_v3$urls_expanded_url, paste, collapse = ", ", character(1L))
data_v3$media_expanded_url <- vapply(data_v3$media_expanded_url, paste, collapse = ", ", character(1L))
data_v3$ext_media_t.co <- vapply(data_v3$ext_media_t.co, paste, collapse = ", ", character(1L))
data_v3$mentions_user_id <- vapply(data_v3$mentions_user_id, paste, collapse = ", ", character(1L))
data_v3$geo_coords <- vapply(data_v3$geo_coords, paste, collapse = ", ", character(1L))
#end - converting list data to character data
#exporting tweets in dataframe "Twitter_data_10_24_v4" to excel sheet
write.csv(data_v3, file="Twitter_data_10_24_v4.csv", row.names = FALSE)

#2nd - dataset
#Scraping tweets as of date 10/26/2019

data_v7 <- search_tweets("(iphone11 OR iphone11pro OR iphone11promax #iphone OR #iphone11 OR shotoniphone OR Appleiphone OR iPhone11camera OR iPhone11unboxing OR
                         Mophie OR slofie OR iphonerepair OR iphonefix OR ios13 OR WirelessCharger
                         OR TimCook OR Cupertino)", n = 5000, include_rts = FALSE, lang = "en", token = twitter_token)
#converting list data to character data
data_v7$urls_url  <- vapply(data_v7$urls_url, paste, collapse = ", ", character(1L))
data_v7$media_url <- vapply(data_v7$media_url, paste, collapse = ", ", character(1L))
data_v7$media_type <- vapply(data_v7$media_type, paste, collapse = ", ", character(1L))
data_v7$ext_media_expanded_url <- vapply(data_v7$ext_media_expanded_url, paste, collapse = ", ", character(1L))
data_v7$mentions_screen_name <- vapply(data_v7$mentions_screen_name, paste, collapse = ", ", character(1L))
data_v7$coords_coords <- vapply(data_v7$coords_coords, paste, collapse = ", ", character(1L))
data_v7$hashtags <- vapply(data_v7$hashtags, paste, collapse = ", ", character(1L))
data_v7$urls_t.co <- vapply(data_v7$urls_t.co, paste, collapse = ", ", character(1L))
data_v7$media_t.co <- vapply(data_v7$media_t.co, paste, collapse = ", ", character(1L))
data_v7$ext_media_url <- vapply(data_v7$ext_media_url, paste, collapse = ", ", character(1L))
data_v7$bbox_coords <- vapply(data_v7$bbox_coords, paste, collapse = ", ", character(1L))
data_v7$symbols <- vapply(data_v7$symbols, paste, collapse = ", ", character(1L))
data_v7$urls_expanded_url <- vapply(data_v7$urls_expanded_url, paste, collapse = ", ", character(1L))
data_v7$media_expanded_url <- vapply(data_v7$media_expanded_url, paste, collapse = ", ", character(1L))
data_v7$ext_media_t.co <- vapply(data_v7$ext_media_t.co, paste, collapse = ", ", character(1L))
data_v7$mentions_user_id <- vapply(data_v7$mentions_user_id, paste, collapse = ", ", character(1L))
data_v7$geo_coords <- vapply(data_v7$geo_coords, paste, collapse = ", ", character(1L))
#data_v7$text
#exporting tweets in dataframe "Twitter_data_10_26_v4" to excel sheet
write.csv(data_v7, file="Twitter_data_10_26_v4.csv", row.names = FALSE)


#3rd - dataset
#Scraping tweets as of date 10/26/2019

#Tim Cook Replies
at_rdt <- search_tweets("to:@tim_cook", n = 5e6, retryonratelimit = TRUE)

#converting list data to character data
at_rdt$urls_url  <- vapply(at_rdt$urls_url, paste, collapse = ", ", character(1L))
at_rdt$media_url <- vapply(at_rdt$media_url, paste, collapse = ", ", character(1L))
at_rdt$media_type <- vapply(at_rdt$media_type, paste, collapse = ", ", character(1L))
at_rdt$ext_media_expanded_url <- vapply(at_rdt$ext_media_expanded_url, paste, collapse = ", ", character(1L))
at_rdt$mentions_screen_name <- vapply(at_rdt$mentions_screen_name, paste, collapse = ", ", character(1L))
at_rdt$coords_coords <- vapply(at_rdt$coords_coords, paste, collapse = ", ", character(1L))
at_rdt$hashtags <- vapply(at_rdt$hashtags, paste, collapse = ", ", character(1L))
at_rdt$urls_t.co <- vapply(at_rdt$urls_t.co, paste, collapse = ", ", character(1L))
at_rdt$media_t.co <- vapply(at_rdt$media_t.co, paste, collapse = ", ", character(1L))
at_rdt$ext_media_url <- vapply(at_rdt$ext_media_url, paste, collapse = ", ", character(1L))
at_rdt$bbox_coords <- vapply(at_rdt$bbox_coords, paste, collapse = ", ", character(1L))
at_rdt$symbols <- vapply(at_rdt$symbols, paste, collapse = ", ", character(1L))
at_rdt$urls_expanded_url <- vapply(at_rdt$urls_expanded_url, paste, collapse = ", ", character(1L))
at_rdt$media_expanded_url <- vapply(at_rdt$media_expanded_url, paste, collapse = ", ", character(1L))
at_rdt$ext_media_t.co <- vapply(at_rdt$ext_media_t.co, paste, collapse = ", ", character(1L))
at_rdt$mentions_user_id <- vapply(at_rdt$mentions_user_id, paste, collapse = ", ", character(1L))
at_rdt$geo_coords <- vapply(at_rdt$geo_coords, paste, collapse = ", ", character(1L))

#exporting tweets in dataframe "Twitter_data_10_26_v4" to excel sheet
write.csv(at_rdt, file="Twitter_replies_TimCook.csv", row.names = FALSE)


# reading all the 3 data sets
df4 <- read.csv("Twitter_data_10_24_v4.csv",header = TRUE, sep = "," ,stringsAsFactors = FALSE)
df5 <- read.csv("Twitter_data_10_26_v4.csv",header = TRUE, sep = "," ,stringsAsFactors = FALSE)
df6 <- read.csv("Twitter_replies_TimCook.csv",header = TRUE, sep = "," ,stringsAsFactors = FALSE)

#Appending all the 3 datasets into one single master data sheet
df_final_data <- rbind(df4,df5,df6)
#taking only unique tweets from the data and loading it to a file.
unique_tweets <- unique(df_final_data$text)
tweet_2 <- data.frame(unique_tweets, stringsAsFactors = FALSE)
names(tweet_2) <- c("text")
#removing the URLs from the data
tweet_2$text <- rm_url(tweet_2$text, pattern=pastex("@rm_twitter_url", "@rm_url"))

# keeping required columns in final dataframe
keep <- c("user_id", "created_at", "screen_name","text", "source", "is_retweet", "favorite_count", "retweet_count", "reply_count", "hashtags", "place_full_name",
          "place_type", "country", "country_code","location", "description", "followers_count", "account_lang" )

xy = df[keep]
final_data <- xy[!duplicated(xy[,c('text')]),]

#exporting final tweets data into excel - to be used for further analysis
write.csv(final_data, file="final_data_tweets.csv", row.names = FALSE)

#Reading final tweets data
final_tweet <- read.csv("final_data_tweets.csv",header = TRUE, sep = "," ,stringsAsFactors = FALSE)
names(final_tweet)

#Pulling unique tweets from final tweets data 
unique_tweet <- read.csv("unique_tweets.CSV",header = TRUE, sep = "," ,stringsAsFactors = FALSE)

#removing the URL from the two data sets --> line89 and line 262
unique_tweet$text <- rm_url(unique_tweet$text, pattern=pastex("@rm_twitter_url", "@rm_url"))
final_tweet$text <- rm_url(final_tweet$text, pattern=pastex("@rm_twitter_url", "@rm_url"))

#length of texts in each respective data sets
length(unique_tweet$text) #5157
length(final_tweet$text) #7287

#exporting only text data from two data sets
write.csv(unique_tweet$text, "unique_text.csv", row.names = FALSE)
write.csv(final_tweet$text, "final_text.csv", row.names = FALSE)

#reading text files 
unique_text <- read.csv("unique_text.CSV",header = TRUE, sep = "," ,stringsAsFactors = FALSE)
final_text <- read.csv("final_text.CSV",header = TRUE, sep = "," ,stringsAsFactors = FALSE)

#Exorting complete text data of all tweets scraped after the iphone11 event
total_text <- rbind(unique_text,final_text)
names(total_text) <- c("text")
write.csv(total_text, "total_text.csv", row.names = FALSE)

#Now the complete text data is ready and we need to clean that extensively for deep analysis
# *** Cleaning the final text data ***/

total_text$text = gsub("&amp", "", total_text$text)
total_text$text = gsub("&amp", "", total_text$text)
total_text$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", total_text$text)
total_text$text = gsub("@\\w+", "", total_text$text)
total_text$text = gsub("[[:punct:]]", "", total_text$text)
total_text$text = gsub("[[:digit:]]", "", total_text$text)
total_text$text = gsub("http\\w+", "", total_text$text)
total_text$text = gsub("[ \t]{2,}", "", total_text$text)
total_text$text = gsub("^\\s+|\\s+$", "", total_text$text)

View(total_text)

write.csv(total_text, "total_text_final.csv", row.names = FALSE)

#Creating the Corpus
corpusiPhone = Corpus(VectorSource(total_text$text))
#Converting to Lower Case
corpusiPhone = tm_map(corpusiPhone, tolower)
#Remove Puntuation
corpusiPhone = tm_map(corpusiPhone, removePunctuation)
# Remove stopwords, the, and, that, for, you etc
corpusiPhone = tm_map(corpusiPhone, removeWords, c(stopwords("english"),"iphone11","appl","iphon","iphone","apple","iphone11pro","iphone11promax","get","amp","now","dont","good","buy",
                                                   "take","need","keep","nba","must","can", "phone", "just", "lastipados",
                                                   "device","iphonepromax"))

# Remove extra WhiteSpaces
corpusiPhone = tm_map(corpusiPhone, stripWhitespace)
# Stem Document
corpusiPhone = tm_map(corpusiPhone, stemDocument)
#Creating WordCloud
wordcloud(corpusiPhone,colors=rainbow(7),max.words=150)
#Create Document term matrix
frequenciesiPhone = DocumentTermMatrix(corpusiPhone)
frequenciesiPhone
#Remove Sparse Terms
iPhoneSparse = removeSparseTerms(frequenciesiPhone, 0.995)
#convert to a Data Frame
iPhoneSparse = as.data.frame(as.matrix(iPhoneSparse))
#Make all Column / Variable names R-friendly
colnames(iPhoneSparse) = make.names(colnames(iPhoneSparse))

#Getting the sentiment value from the tweets
sent.value <- get_sentiment(total_text$text)

#making the categorical variable of the sentiment values
category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
iPhoneSparse$Polarity = category_senti
table(iPhoneSparse$Polarity)




############################################################################
########### Text mining and sentiment Analysis #############################
############################################################################


#importing cleaned twitter text which is collected over 4 weeks.
#total_text_final.CSV -> aggregate of 4 weeks tweets data
tweets_data <- read.csv("total_text_final.CSV",header = TRUE, sep = "," ,stringsAsFactors = FALSE)
names(tweets_data)
summary(tweets_data)


#************ Descriptive graphs ********

#Reading final tweets data having 18 attributes --- performing descriptive analysis and visualizations
descr_tweet <- read.csv("final_data_tweets.CSV",header = TRUE, sep = "," ,stringsAsFactors = FALSE)
names(descr_tweet)
str(descr_tweet)

src_df <- table(descr_tweet$source)
str(src_df)

w <- as.data.frame(table(descr_tweet$source))
w <- w[order(-w$Freq),]
names(w) <- c("Source","Frequency")



# visualization -- source attribute of tweets 
ggplot(data=w[0:10,], aes(x=reorder(Source,-Frequency), y=Frequency)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Frequency), vjust=-0.3, size=3.5)+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +xlab("Source")


w <- as.data.frame(table(descr_tweet$country ))

w <- w[order(-w$Freq),]
names(w) <- c("Country","Frequency")


#Visualization - Country attribute in tweets
ggplot(data=w[0:10,], aes(x=reorder(Country,-Frequency), y=Frequency)) +
  geom_bar(stat="identity", fill="orange")+
  geom_text(aes(label=Frequency), vjust=-0.3, size=3.5)+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +xlab("Source")


#*************************
#number of tweets through out the day

descr_tweet %>%
  mutate(created_at=substr(created_at, 12, 16)) %>%
  count(created_at) %>%
  ggplot(aes(x=as.numeric(as.factor(created_at)), y=n, group=1)) +
  geom_line(size=1, show.legend=FALSE,color = "skyblue") +
  labs(x="UCT time", y="Number of Tweets") + 
  theme_bw()

#*****************



#selecting only text from the tweets
iphone_tweets = tweets_data %>% select(text)

#converting the text data to charecter
iphone_tweets$text = as.character(iphone_tweets$text)

#unnesting the tokens
tidy_dataset = iphone_tweets %>% unnest_tokens(word, text)

#arranging the frequently occuring words in descending order before joining stop words.
tidy_dataset %>% count(word) %>%  arrange(desc(n))

#list of stop words
data("stop_words")

#cleaning the tidy data with the stop words.
tidy_dataset2 = tidy_dataset %>%  anti_join(stop_words)

#arranging the stop words in descending order after removing the stop words.
tidy_dataset2 %>% count(word) %>%   arrange(desc(n))

#Regex pattern
patterndigits = '\\b[0-9]+\\b'

#removing the digits from text data
tidy_dataset2$word = tidy_dataset2$word %>% str_replace_all(patterndigits, '')

#stripping white space
tidy_dataset2$word = tidy_dataset2$word%>% str_replace_all('[:space:]','')

#removing the blanks
tidy_dataset3 = tidy_dataset2%>%  filter(!(word==''))

#arranging the words in descending order
tidy_dataset3 %>% count(word)%>% arrange(desc(n))

#filtering words with occurance less than 0.5
frequency = tidy_dataset3%>%  count(word)%>%  arrange(desc(n))%>%
  mutate(proportion = (n/sum(n)*100))%>%  filter(proportion>=0.5)


#Plotting the graph between -> Word Proportion (vs) Word
ggplot(frequency, aes(x = proportion, y = word)) + 
  geom_abline(color = "gray40", lty = 2) + 
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") + 
  labs(y = 'Word', x = 'Proportion')


#update the list with the frequent iphone words
list = c("iphone11","iphone11promax","iphone11pro","#iphone11",
         "Appleiphone","iphone11camera","shotoniphone11","ios13")

#removing the frequent occuring words from the text data
tidy_dataset3 = tidy_dataset3 %>% filter(!(word %in% list))

#stemming the words, to group similar words together.
tidy_dataset4 = tidy_dataset3 %>% mutate_at("word", funs(wordStem((.), language="en")))

tidy_dataset4 = tidy_dataset3 %>%   mutate_at("word", funs(wordStem((.), language="en")))

#Obersrving the frequent list of words after removing frequent words.
tidy_dataset4 %>% count(word) %>% arrange(desc(n))


#filtering words with occurance less than 0.5
frequency2 = tidy_dataset4 %>% 
  count(word) %>% arrange(desc(n)) %>% 
  mutate(proportion = (n / sum(n)*100)) %>% 
  filter(proportion >= 0.5)

#plotting graph between Word proportion (vs) Word after stemming
ggplot(frequency2, aes(x = proportion, y = word)) + 
  geom_abline(color = "gray40", lty = 2) + 
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") + 
  theme(legend.position="none") + 
  labs(y = 'Word', x = 'Proportion')


#Sentiments from "bing"
get_sentiments("bing") %>% distinct(sentiment)

#sentiments from "nrc"
get_sentiments('nrc') %>% distinct(sentiment)

#Joining the word by its sentiment
tidy_dataset4 %>% inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>% spread(sentiment, n, fill = 0) %>% 
  mutate(diffsent = positive - negative)

#filtering words with sentiment as joy and sad
nrc_joysad = get_sentiments('nrc') %>% 
  filter(sentiment == 'joy' | sentiment == 'sadness')

nrow(nrc_joysad)


(tweet_joysad = tidy_dataset4 %>%
    inner_join(nrc_joysad) %>% 
    count(word, sentiment) %>% 
    spread(sentiment, n, fill = 0) %>% 
    mutate(contentment = joy - sadness, linenumber = row_number()) %>% 
    arrange(desc(contentment)))


#plotting the graph for above joy and sadness
ggplot(tweet_joysad, aes(x=linenumber, y=contentment)) + 
  coord_flip() +  theme_light(base_size = 15) + 
  labs(x='Index Value', y='Contentment') +
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)
  ) +
  geom_col()


#Brief data on joy and sad words
(tweet_joysad2 = tweet_joysad %>% slice(1:10,253:262))

#PLotting the graph
ggplot(tweet_joysad2, aes(x=linenumber, y=contentment, fill=word)) + 
  coord_flip() + theme_light(base_size = 15) +
  labs(x='Index Value', y='Contentment'
  ) + 
  theme(
    legend.position = 'bottom',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) + 
  geom_col()


#filtering the data with only sadness and surprise
nrc_sadsurp = get_sentiments('nrc') %>% 
  filter(sentiment == 'sadness' |sentiment == 'surprise')

nrow(nrc_sadsurp)

(tweet_sadsurp = tidy_dataset4 %>% 
    inner_join(nrc_sadsurp) %>%
    count(word, sentiment) %>% 
    spread(sentiment, n, fill = 0) %>% 
    mutate(trustworthy = sadness - surprise, linenumber = row_number()) %>% 
    arrange(desc(trustworthy)) %>% 
    slice(1:10,348:357))

#plotting the graph between sadness and surprise
ggplot(tweet_sadsurp, aes(x=linenumber, y=trustworthy, fill=word)) +
  coord_flip() + 
  theme_light(base_size = 15) + 
  labs( 
    x='Index Value', 
    y='Trustworthiness' 
  ) +
  theme( 
    legend.position = 'bottom', 
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  geom_col()

iphone_tweets %>% 
  filter(str_detect(text, 'case|Case')) %>% 
  select(text)

iphone_tweets %>% 
  filter(str_detect(text, 'fall|Fall')) %>% 
  select(text)

#sadness vs suprise wordcloud
tidy_dataset4 %>% 
  inner_join(nrc_sadsurp) %>% 
  count(word, sentiment) %>% 
  slice(1:40,318:357) %>% 
  acast(word~sentiment, value.var='n',fill=0) %>% 
  comparison.cloud(colors=c('gray30','gray70'))

# Download if necessary
ud_model = udpipe_download_model(language = "english")

tidy_post1 = tidy_dataset4 %>%   select(word)

ud_model = udpipe_load_model(ud_model$file_model)
x = as.data.frame(udpipe_annotate(ud_model, x = tidy_post1$word))

post_stats = txt_freq(x$upos)
post_stats$key = factor(post_stats$key, levels = rev(post_stats$key))

ggplot(post_stats, aes(x=key, y=as.factor(freq), fill=key)) + 
  coord_flip() + theme_light(base_size = 15) +
  labs( x='Frequency',  y='',
        title='UPOS (Universal Parts of Speech)'
  ) + 
  theme( 
    legend.position = 'none', 
    panel.grid = element_blank(),
    axis.title = element_text(size = 10), 
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    title = element_text(size = 13)
  ) +
  geom_col() + 
  scale_fill_grey()

#noun graphs
noun_stats = subset(x, upos %in% c("NOUN"))
noun_stats2 = txt_freq(noun_stats$token)
noun_stats2$key = factor(noun_stats2$key, levels = rev(noun_stats2$key))
noun_stats2 %>% 
  slice(1:20) %>% 
  ggplot(aes(x=key, y=as.factor(freq), fill=freq)) + 
  coord_flip() +
  theme_light(base_size = 15) + 
  labs(
    x='Frequency', 
    y='',
    title='Noun Occurrences' 
  ) + 
  theme( 
    legend.position = 'none', 
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    title = element_text(size = 13)
  ) + 
  scale_fill_gradient(low="orange", high="orange3") + 
  geom_col()


#adjective graphs
adjstats = subset(x, upos %in% c("ADJ"))
adjstats2 = txt_freq(adjstats$token)
adjstats2$key = factor(adjstats2$key, levels = rev(adjstats2$key))
adjstats2 %>% 
  slice(1:20) %>% 
  ggplot(aes(x=key, y=as.factor(freq), fill=freq)) + 
  coord_flip() + 
  theme_light(base_size = 15) + 
  labs( 
    x='Frequency', 
    y='',
    title='Adjectives Occurrences' 
  ) +
  theme(legend.position = 'none', 
        panel.grid = element_blank(),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        title = element_text(size = 13)
  ) +
  scale_fill_gradient(low="chartreuse", high="chartreuse3") +
  geom_col()



#verb occurunces
verbstats = subset(x, upos %in% c("VERB"))
verbstats2 = txt_freq(verbstats$token)
verbstats2$key = factor(verbstats2$key, levels = rev(verbstats2$key))
verbstats2 %>% 
  slice(1:20) %>% 
  ggplot(aes(x=key, y=as.factor(freq), fill=freq)) + 
  coord_flip() + 
  theme_light(base_size = 15) + 
  labs( x='Frequency', y='',
        title='Verb Occurrences'
  ) +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(), 
    axis.title = element_text(size = 10), 
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10), 
    title = element_text(size = 13)
  ) +
  scale_fill_gradient(low="tan", high="tan3") +
  geom_col()


# Tokens
tokens <- tweets_data %>%  
  unnest_tokens(word, text) %>%
  select(word)

# Positive and negative words 
tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=c("#F8766D", "#00BFC4"), max.words=200)

# Sentiments and frequency associated with each word  
sentiments <- tokens %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) 

# Frequency of each sentiment
ggplot(data=sentiments, aes(x=reorder(sentiment, n, sum), y=n)) + 
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
  labs(x="Sentiment", y="Frequency") +
  theme_bw() +
  coord_flip()

# ****************
# Top 10 frequent terms for each sentiment
sentiments %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Frequency", x="Words") +
  coord_flip() 



############################################################################
######### Phone comparision tweets text mining & Analysis ##################
############################################################################


tweets_compare <- read.csv("total_text_final.CSV",header = TRUE, sep = "," ,stringsAsFactors = FALSE)
names(tweets_compare)
summary(tweets_compare)

#selecting only text from the tweets
iphone_compare= tweets_compare %>% select(text)
iphone_competitor <- iphone_compare %>% 
  filter(str_detect(text, 'samsung|Samsusng|Google|google|pixel|android|galaxi')) %>% 
  select(text)

#converting the text data to charecter
iphone_competitor$text = as.character(iphone_competitor$text)

#unnesting the tokens
tidy_dataset = iphone_competitor %>% unnest_tokens(word, text)

#arranging the frequently occuring words in descending order before joining stop words.
tidy_dataset %>% count(word) %>%  arrange(desc(n))


#list of stop words
data("stop_words")

#cleaning the tidy data with the stop words.
tidy_dataset2 = tidy_dataset %>%  anti_join(stop_words)


#arranging the stop words in descending order after removing the stop words.
tidy_dataset2 %>% count(word) %>%   arrange(desc(n))

patterndigits = '\\b[0-9]+\\b'

#removing the digits from text data
tidy_dataset2$word = tidy_dataset2$word %>% str_replace_all(patterndigits, '')

#stripping white space
tidy_dataset2$word = tidy_dataset2$word%>% str_replace_all('[:space:]','')

#removing the blanks
tidy_dataset3 = tidy_dataset2%>%  filter(!(word==''))

#arranging the words in descending order
tidy_dataset3 %>% count(word)%>% arrange(desc(n))

#filtering words with occurance less than 0.5
frequency = tidy_dataset3%>%  count(word)%>%  arrange(desc(n))%>%
  mutate(proportion = (n/sum(n)*100))%>%  filter(proportion>=0.5)


#Plotting the graph between -> Word Proportion (vs) Word
ggplot(frequency, aes(x = proportion, y = word)) + 
  geom_abline(color = "gray40", lty = 2) + 
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") + 
  labs(y = 'Word', x = 'Proportion')

#update the list with the frequent iphone words
list = c("samsung","google","pixel","android","iphone","iphonex","iphonepromax","galaxi","iphonepro")




#removing the frequent occuring words from the text data
tidy_dataset3 = tidy_dataset3 %>% filter(!(word %in% list))
str(tidy_dataset3)

#stemming the words, to group similar words together.
tidy_dataset4 = tidy_dataset3 %>% mutate_at("word", funs(wordStem((.), language="en")))

tidy_dataset4 = tidy_dataset3 %>%   mutate_at("word", funs(wordStem((.), language="en")))

#Obersrving the frequent list of words after removing frequent words.
tidy_dataset4 %>% count(word) %>% arrange(desc(n))


#filtering words with occurance less than 0.5
frequency2 = tidy_dataset4 %>% 
  count(word) %>% arrange(desc(n)) %>% 
  mutate(proportion = (n / sum(n)*100)) %>% 
  filter(proportion >= 0.5)

#plotting graph between Word proportion (vs) Word after stemming
ggplot(frequency2, aes(x = proportion, y = word)) + 
  geom_abline(color = "gray40", lty = 2) + 
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") + 
  theme(legend.position="none") + 
  labs(y = 'Word', x = 'Proportion')


#Sentiments from "bing"
get_sentiments("bing") %>% distinct(sentiment)

#sentiments from "nrc"
get_sentiments('nrc') %>% distinct(sentiment)

#Joining the word by its sentiment
tidy_dataset4 %>% inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>% spread(sentiment, n, fill = 0) %>% 
  mutate(diffsent = positive - negative)

#filtering words with sentiment as joy and sad
nrc_joysad = get_sentiments('nrc') %>% 
  filter(sentiment == 'joy' | sentiment == 'sadness')

nrow(nrc_joysad)


(tweet_joysad = tidy_dataset4 %>%
    inner_join(nrc_joysad) %>% 
    count(word, sentiment) %>% 
    spread(sentiment, n, fill = 0) %>% 
    mutate(contentment = joy - sadness, linenumber = row_number()) %>% 
    arrange(desc(contentment)))


#plotting the graph for above joy and sadness
ggplot(tweet_joysad, aes(x=linenumber, y=contentment)) + 
  coord_flip() +  theme_light(base_size = 15) + 
  labs(x='Index Value', y='Contentment') +
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)
  ) +
  geom_col()

#Brief data on joy and sad words
(tweet_joysad2 = tweet_joysad %>% slice(1:10,253:262))

#PLotting the graph
ggplot(tweet_joysad2, aes(x=linenumber, y=contentment, fill=word)) + 
  coord_flip() + theme_light(base_size = 15) +
  labs(x='Index Value', y='Contentment'
  ) + 
  theme(
    legend.position = 'bottom',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) + 
  geom_col()



####################         END       ###################




