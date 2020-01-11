# -*- coding: utf-8 -*-
"""
Created on Sat Dec  7 00:53:51 2019

@author: nikhil­_gunti
"""


#Importing necessary packages for the successful code run
import pandas as pd
import matplotlib.pyplot as plt
import os
import nltk
import regex as re
from nltk.stem import PorterStemmer
import wordcloud
nltk.download('stopwords')
from nltk import word_tokenize, sent_tokenize
from nltk.corpus import stopwords
from nltk.stem import LancasterStemmer, WordNetLemmatizer, PorterStemmer
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.decomposition import LatentDirichletAllocation
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import NMF
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score
from nltk import word_tokenize, pos_tag, ne_chunk
from nltk.chunk import conlltags2tree, tree2conlltags

#Setting up work directory
os.chdir("C:\\Users\\hp\\Desktop\\MSIS 5600\\Twitter_data_project\\Anirud codes")

#Reading final tweets text data excel file
tweets_data = pd.read_csv('total_text_final.csv', engine='python')
tweets_data.columns

#Renaming 'text' column to 'tweettext' in the csv file read into python
tweets_data.rename(columns={'text': 'tweettext'}, inplace=True)

#Converting the text to lowercase so that all words with similar spelling are treated the same
tweets_data['tweettext'] = tweets_data['tweettext'].apply(lambda x: " ".join(x.lower() for x in x.split()))
tweets_data['tweettext'][2]

#Removing numerical values and punctuation from the words
patterndigits = '\\b[0-9]+\\b'
tweets_data['tweettext'] = tweets_data['tweettext'].str.replace(patterndigits,'')
patternpunc = '[^\w\s]'
tweets_data['tweettext'] = tweets_data['tweettext'].str.replace(patternpunc,'')

# No more punctuation or numerical text!
tweets_data['tweettext'][2]

#Removing stop words from the data by using the library nltk
stop = stopwords.words('english')

# Before removal of stopwords
tweets_data['tweettext'][2]
tweets_data['tweettext'] = tweets_data['tweettext'].apply(lambda x: " ".join(x for x in x.split() if x not in stop))

# After removal of stopwords
tweets_data['tweettext'][2]

#Removing the Twitter account names of each phone names from the data
phone_names = ['iphone11','iphone11pro','iphone11promax']
tweets_data['tweettext'] = tweets_data['tweettext'].apply(lambda x: " ".join(x for x in x.split() if x not in phone_names))

#Stemming the words by using PorterStemmer() on the data.
porstem = PorterStemmer()
tweets_data['tweettext'] = tweets_data['tweettext'].apply(lambda x: " ".join([porstem.stem(word) for word in x.split()]))
tweets_data['tweettext'][2]

#Creating a document-term matrix. We use a vectorizer from scikitlearn
from sklearn.feature_extraction.text import CountVectorizer
vectorizer = CountVectorizer()

tokens_data = pd.DataFrame(vectorizer.fit_transform(tweets_data['tweettext']).toarray(), columns=vectorizer.get_feature_names())

#Finding out number of tweets delay more than once
delayproblems = tokens_data[(tokens_data.price>1)]

delayproblems['price']

#Setting value that term appear 80% of documents, minimum number of documents
    # a word must appear in to be counted=4
vectorizer = CountVectorizer(max_df=0.8, min_df=4, stop_words='english')
doc_term_matrix = vectorizer.fit_transform(tweets_data['tweettext'].values.astype('U'))
doc_term_matrix

#Using DTM, generating Latent Dirichlet allocation (LDA) with 5 topics and 
    # setting the seed to 35 so that the results are consistent each time this is run.
LDA = LatentDirichletAllocation(n_components=5, random_state=35)
LDA.fit(doc_term_matrix)

#Retrieving words in the first topic, sort the indexes according to 
    #probability values using argsort(), and output the results to the console.
first_topic = LDA.components_[0]
top_topic_words = first_topic.argsort()[-10:]

for i in top_topic_words:
   print(vectorizer.get_feature_names()[i])

#printing 10 top words for each of the topics
for i,topic in enumerate(LDA.components_):
   print(f'Top 10 words for topic #{i}:')
   print([vectorizer.get_feature_names()[i] for i in topic.argsort()[-10:]])
   print('\n')

#Assigning a probability of a topic belonging to each document
   #limiting the topic to the one with greatest value
topic_values = LDA.transform(doc_term_matrix)
topic_values.shape
tweets_data['topic'] = topic_values.argmax(axis=1)
tweets_data.head()

#Creating a document-term matrix using TF-IDF
tfidf_vect = TfidfVectorizer(max_df=0.8, min_df=5, stop_words='english')
doc_term_matrix2 = tfidf_vect.fit_transform(tweets_data['tweettext'].values.astype('U'))
nmf = NMF(n_components=5, random_state=42)
nmf.fit(doc_term_matrix2)

#In Non-Negative Matrix Factorization (NMF), pulling the list of words for the first topic
first_topic = nmf.components_[0]
top_topic_words = first_topic.argsort()[-10:]

for i in top_topic_words:
   print(tfidf_vect.get_feature_names()[i])
   
#Extracting the top 10 words of each topic and then adding a new column to the dataframe with the topic value.
for i,topic in enumerate(nmf.components_):
   print(f'Top 10 words for topic #{i}:')
   print([tfidf_vect.get_feature_names()[i] for i in topic.argsort()[-10:]])
   print('\n')   


topic_values2 = nmf.transform(doc_term_matrix2)
tweets_data['topic2'] = topic_values2.argmax(axis=1)
tweets_data.head()


##################################################################################
##########                  Named Entity                             #############
##################################################################################

#Reading final tweets text  excel file for Named Entity recognition
ner_tweet = pd.read_table('total_text_final.csv', sep = ',', engine='python')
#printing top 5 rows from the above read table
ner_tweet.head()
#Seperating text with the help of seperator 
text_sep = ner_tweet.text.str.cat(sep=' ')

#Tokenizing the data and converting it into Parts of Speech Tagging elements
post1 = pos_tag(word_tokenize(text_sep))
print(post1)

#Running the POST elements through a named entity chunker
tree1 = ne_chunk(post1)
print(tree1)

#Extracting named-entity labels among POST based on their labels
entityp = []
entityo = []
entityg = []
entitydesc = []

#Looping through tree structure, identifying elements by their label and saves them to a vector or list
for x in str(tree1).split('\n'):
    if 'PERSON' in x:
        entityp.append(x)
    elif 'ORGANIZATION' in x:
        entityo.append(x)
    elif 'GPE' in x or 'GSP' in x:
        entityg.append(x)
    elif '/NN' in x:
        entitydesc.append(x)

entityp
entityo
entityg
entitydesc

#Converting tree structure into inside-out-begin(IOB) tagging format
iob_tag = tree2conlltags(tree1)
print(iob_tag)

#Filtering unnecessary words and extracting required keywords
#Person entities
entityp1 = re.sub(r'/NNP','', str(entityp))
entityp1 = re.sub(r'/NNPS','', entityp1)
entityp1 = re.sub(r'/JJ','', entityp1)
entityp1 = re.sub(r'GPE','', entityp1)
entityp1 = re.sub(r'PERSON','', entityp1)
#Defining stopwords to be removed from our overall set of words
stopwords = set(STOPWORDS)
#Building wordcloud for person entities
wordcloud = WordCloud(
       background_color='white',
       stopwords=stopwords,
       max_words=200,
       max_font_size=40, 
       scale=3,
   ).generate(str(entityp1))

plt.figure(1, figsize=(16, 16))
plt.axis('off')
plt.imshow(wordcloud)
plt.show()


#Organization entites
entityo1 = re.sub(r'/NNP','', str(entityo))
entityo1 = re.sub(r'/NNPS','', entityo1)
entityo1 = re.sub(r'/JJ','', entityo1)
entityo1 = re.sub(r'GPE','', entityo1)
entityo1 = re.sub(r'ORGANIZATION','', entityo1)
#Adding customized stop words into our list based on our observation
stopwords = set(STOPWORDS)
stopwords.add('iPhone')
stopwords.add('NN')
stopwords.add('iPhoneProMax')
stopwords.add('iPhonePro')
stopwords.add('Apple')
#Building wordcloud for organization entities
wordcloud = WordCloud(
       background_color='white',
       stopwords=stopwords,
       max_words=200,
       max_font_size=40, 
       scale=3,
   ).generate(str(entityo1))

plt.figure(1, figsize=(16, 16))
plt.axis('off')
   
plt.imshow(wordcloud)
plt.show()


#Geographiics - Location Entites
entityg1 = re.sub(r'/NNP','', str(entityg))
entityg1 = re.sub(r'/NNPS','', entityg1)
entityg1 = re.sub(r'/JJ','', entityg1)
entityg1 = re.sub(r'GPE','', entityg1)
#Building wordcloud for location entities
stopwords = set(STOPWORDS)
wordcloud = WordCloud(
       background_color='white',
       stopwords=stopwords,
       max_words=200,
       max_font_size=40, 
       scale=3,
   ).generate(str(entityg1))

plt.figure(1, figsize=(16, 16))
plt.axis('off')
   
plt.imshow(wordcloud)
plt.show()