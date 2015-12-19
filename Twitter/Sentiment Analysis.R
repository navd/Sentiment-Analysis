# required pakacges
# install.packages("Rstem")
# install.packages("tm")
# install.packages("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz", repo=NULL, type="source")
# install.packages("twitteR")
# install.packages("ROAuth")
# install.packages(c("devtools", "rjson", "bit64", "httr"))
#install.packages("rjson")
#install.packages("MASS")
library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library("ROAuth")
library("rjson")
library("httr")
library("devtools")
library("bit64")
#install.packages("rjson")

# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

#create an object "cred" that will save the authenticated object that we can use for later sessions
consumerKey<-"p6KNwzh98FnSGEYehRgLumT2u"
consumerSecret<-"NHft3vE3qjDyParpX1cOVsRxGYLRJDZG9EWVHleyYM08rIvVAs"
requestURL<-"https://api.twitter.com/oauth/request_token"
accessURL<-"https://api.twitter.com/oauth/access_token"
authURL<-"https://api.twitter.com/oauth/authorize"
access_token<-"456361928-ZdLwRJx4Ljbvhh8oBKOW6sGV2avTolb6ceb2QwnA"
access_secret<-"qWtW3JxRzhFho3JOf0d1k733veajV7Zp2F99GtqIOjdiz"

setup_twitter_oauth(consumerKey, consumerSecret, access_token, access_secret)
searchResults <- searchTwitter("#parisAttack", n=1500, since = as.character(Sys.Date()-30), until = as.character(Sys.Date()))
#head(searchResults)
some_txt = sapply(searchResults, function(x) x$getText())
#head(some_txt)
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)


try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
#View(class_emo)
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]


# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets")

  # separating text by emotion
  emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = some_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)
