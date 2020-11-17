# Twitter-analysis
Twitter analysis of COVID-19 experts

---
output:
  html_document: default
  word_document: default
---

#######Organizations

###Organization 1: GHS

```{r}


library(tidyverse)
library(rtweet)
library(dplyr)
library(tidytext)
library(tm)
library(tmap)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(corpus)
library(syuzhet)



twitter_token <- create_token(
  app = "Miladn5",
  

GHS_tweets <- get_timeline("GHS", n= 6400)

n0=count(GHS_tweets)


GHS_tweets_FC <- GHS_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
GHS_tweets_FC 
mutate(GHS_tweets,GHS_tweets_FC )
write_csv(GHS_tweets)

GHS_tweets_RC <- GHS_tweets %>% arrange(-retweet_count)%>% select(retweet_count)
GHS_tweets_RC


summary(GHS_tweets)


# Remove retweets
GHS_tweets_organic <- GHS_tweets[GHS_tweets$is_retweet==FALSE, ] 
#print(GHS_tweets_organic)

# Remove replies
GHS_tweets_organic <- subset(GHS_tweets_organic, is.na(GHS_tweets_organic$reply_to_status_id))


GHS_tweets_organic_FC <- GHS_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)
#GHS_tweets_organic[1,5]
print(GHS_tweets_organic_FC)

GHS_tweets_organic_RC <- GHS_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)
#GHS_tweets_organic[1,5]
print(GHS_tweets_organic_RC)

summary(GHS_tweets_organic)
n1=count(GHS_tweets_organic)


# Keeping only the retweets
GHS_retweets <- GHS_tweets[GHS_tweets$is_retweet==TRUE,]
n2=count(GHS_retweets)


# Keeping only the replies
GHS_replies <- subset(GHS_tweets, !is.na(GHS_tweets$reply_to_status_id))
n3=count(GHS_replies)

c(n0,n1,n2,n3)

# Creating a data frame
#data <- data.frame(
 # category=c("Organic", "Retweets", "Replies"),
 # count=c(1910, 1000, 335))
#print(data)

# Adding columns 
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))


# Rounding the data to two decimal points
#data <- round_df(data, 2)
# Specify what the legend should say
Type_of_Tweet <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


colnames(GHS_tweets)[colnames(GHS_tweets)=="screen_name"] <- "Twitter_Account" 



ts_plot(dplyr::group_by(GHS_tweets, Twitter_Account), "year") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from GHS",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


GHS_app <- GHS_tweets %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
GHS_app <- subset(GHS_app, count > 11)
print(GHS_app)

data <- data.frame(
  category=GHS_app$source,
  count=GHS_app$count
)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
#data <- round_df(data, 2)
Source <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")



GHS_tweets_organic$text <-  gsub("https\\S*", "", GHS_tweets_organic$text)
GHS_tweets_organic$text <-  gsub("@\\S*", "", GHS_tweets_organic$text) 
GHS_tweets_organic$text  <-  gsub("amp", "", GHS_tweets_organic$text) 
GHS_tweets_organic$text  <-  gsub("[\r\n]", "", GHS_tweets_organic$text)
GHS_tweets_organic$text  <-  gsub("[[:punct:]]", "", GHS_tweets_organic$text)
tweets <- GHS_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets <- tweets %>%
  anti_join(stop_words)

tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of GHS",
       subtitle = "Stop words removed from the list")

tweets %>% count(word, sort = TRUE) %>% top_n(10)



GHS_tweets_organic$hashtags <- as.character(GHS_tweets_organic$hashtags)
GHS_tweets_organic$hashtags <- gsub("c\\(", "", GHS_tweets_organic$hashtags)
set.seed(1234)
wordcloud(GHS_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



set.seed(1234)
wordcloud(GHS_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))


# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
# removing mentions, in case needed
tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

sentimentscores
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()
```



2. _HannahRitchie


```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)
#rate_limit()

HannahRitchie_tweets <- get_timeline("_HannahRitchie", n= 6400)
n0=count(HannahRitchie_tweets)
HannahRitchie_tweets_FC <- HannahRitchie_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
HannahRitchie_tweets_FC 
HannahRitchie_tweets_RC <- HannahRitchie_tweets %>% arrange(-retweet_count)%>% select(retweet_count)
HannahRitchie_tweets_RC
summary(HannahRitchie_tweets)
HannahRitchie_tweets_organic <- HannahRitchie_tweets[HannahRitchie_tweets$is_retweet==FALSE, ] # Remove retweets
HannahRitchie_tweets_organic <- subset(HannahRitchie_tweets_organic, is.na(HannahRitchie_tweets_organic$reply_to_status_id))# Remove replies
HannahRitchie_tweets_organic_FC <- HannahRitchie_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)
print(HannahRitchie_tweets_organic_FC)
HannahRitchie_tweets_organic_RC <- HannahRitchie_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)
print(HannahRitchie_tweets_organic_RC)
summary(HannahRitchie_tweets_organic)
n1=count(HannahRitchie_tweets_organic)
HannahRitchie_retweets <- HannahRitchie_tweets[HannahRitchie_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(HannahRitchie_retweets)
HannahRitchie_replies <- subset(HannahRitchie_tweets, !is.na(HannahRitchie_tweets$reply_to_status_id))# Keeping only the replies
n3=count(HannahRitchie_replies)
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")
colnames(HannahRitchie_tweets)[colnames(HannahRitchie_tweets)=="screen_name"] <- "Twitter_Account" 
ts_plot(dplyr::group_by(HannahRitchie_tweets, Twitter_Account), "year") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from HannahRitchie",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet")
HannahRitchie_app <- HannahRitchie_tweets %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
HannahRitchie_app <- subset(HannahRitchie_app, count > 11)
print(HannahRitchie_app)
data <- data.frame(
  category=HannahRitchie_app$source,
  count=HannahRitchie_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")
HannahRitchie_tweets_organic$text <-  gsub("https\\S*", "", HannahRitchie_tweets_organic$text)
HannahRitchie_tweets_organic$text <-  gsub("@\\S*", "", HannahRitchie_tweets_organic$text) 
HannahRitchie_tweets_organic$text  <-  gsub("amp", "", HannahRitchie_tweets_organic$text) 
HannahRitchie_tweets_organic$text  <-  gsub("[\r\n]", "", HannahRitchie_tweets_organic$text)
HannahRitchie_tweets_organic$text  <-  gsub("[[:punct:]]", "", HannahRitchie_tweets_organic$text)
tweets <- HannahRitchie_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets <- tweets %>%
  anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of HannahRitchie",
       subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
HannahRitchie_tweets_organic$hashtags <- as.character(HannahRitchie_tweets_organic$hashtags)
HannahRitchie_tweets_organic$hashtags <- gsub("c\\(", "", HannahRitchie_tweets_organic$hashtags)
set.seed(1234)
wordcloud(HannahRitchie_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
set.seed(1234)
wordcloud(HannahRitchie_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+  ggtitle("Total sentiment based on scores")+
  theme_minimal()
```

3. AdamJKucharski

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
AdamJKucharski_tweets <- get_timeline("AdamJKucharski", n= 6400)
n0=count(AdamJKucharski_tweets)
AdamJKucharski_tweets_FC <- AdamJKucharski_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
AdamJKucharski_tweets_FC 
AdamJKucharski_tweets_RC <- AdamJKucharski_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
AdamJKucharski_tweets_RC   
summary(AdamJKucharski_tweets)
AdamJKucharski_tweets_organic <- AdamJKucharski_tweets[AdamJKucharski_tweets$is_retweet==FALSE, ] # Remove retweets
AdamJKucharski_tweets_organic <- subset(AdamJKucharski_tweets_organic, is.na(AdamJKucharski_tweets_organic$reply_to_status_id))# Remove replies
AdamJKucharski_tweets_organic_FC <- AdamJKucharski_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(AdamJKucharski_tweets_organic_FC)
AdamJKucharski_tweets_organic_RC <- AdamJKucharski_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
AdamJKucharski_tweets_organic_RC
summary(AdamJKucharski_tweets_organic)
n1=count(AdamJKucharski_tweets_organic)
AdamJKucharski_retweets <- AdamJKucharski_tweets[AdamJKucharski_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(AdamJKucharski_retweets)
AdamJKucharski_replies <- subset(AdamJKucharski_tweets, !is.na(AdamJKucharski_tweets$reply_to_status_id))# Keeping only the replies
n3=count(AdamJKucharski_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
AdamJKucharski_app <- AdamJKucharski_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
AdamJKucharski_app <- subset(AdamJKucharski_app, count > 11)
print(AdamJKucharski_app)
data <- data.frame( category=AdamJKucharski_app$source,count=AdamJKucharski_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
AdamJKucharski_tweets_organic$text <-  gsub("https\\S*", "", AdamJKucharski_tweets_organic$text)
AdamJKucharski_tweets_organic$text <-  gsub("@\\S*", "", AdamJKucharski_tweets_organic$text) 
AdamJKucharski_tweets_organic$text  <-  gsub("amp", "", AdamJKucharski_tweets_organic$text) 
AdamJKucharski_tweets_organic$text  <-  gsub("[\r\n]", "", AdamJKucharski_tweets_organic$text)
AdamJKucharski_tweets_organic$text  <-  gsub("[[:punct:]]", "", AdamJKucharski_tweets_organic$text)
tweets <- AdamJKucharski_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of AdamJKucharski", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
AdamJKucharski_tweets_organic$hashtags <- as.character(AdamJKucharski_tweets_organic$hashtags)
AdamJKucharski_tweets_organic$hashtags <- gsub("c\\(", "", AdamJKucharski_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(AdamJKucharski_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(AdamJKucharski_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores

```

4. aetiology


```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("aetiology", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

5. alexandraphelan

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("alexandraphelan", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```


6. AmeshAA

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("AmeshAA", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

7. ashishkjha

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("ashishkjha", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

8. Atul_Gawande

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("Atul_Gawande", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

9. BillHanage

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("BillHanage", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

10. bogochisaac

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("bogochisaac", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

11. CaulfieldTim

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("CaulfieldTim", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

12. CDCDirector

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("CDCDirector", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

13. celinegounder

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("celinegounder", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

14. chngin_the_wrld

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("chngin_the_wrld", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

15. cmyeaton

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("cmyeaton", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

16. cnnbrk

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("cnnbrk", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```


17. DavidJuurlink

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("DavidJuurlink", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

18. devisridhar

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("devisridhar", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

19. drewaharris


```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("drewaharris", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```


20. DrMikeRyan

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("DrMikeRyan", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

21. DrNancyM_CDC

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("DrNancyM_CDC", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

22. DrPChouinard

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("DrPChouinard", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

23. DrTedros 

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("DrTedros", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

24. DrTomFrieden

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("DrTomFrieden", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

25. epstein_dan

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("epstein_dan", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

26.florian_krammer

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("florian_krammer", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

27. HelenBranswell

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("HelenBranswell", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

28. JAMA_current

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("JAMA_current", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

29. JeremyKonyndyk

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("JeremyKonyndyk", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

30. JohnsHopkinsSPH

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("JohnsHopkinsSPH", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

31. juliaoftoronto

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("juliaoftoronto", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

32. kakape

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("kakape", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

33. MackayIM

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("MackayIM", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

34. MaxCRoser

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("MaxCRoser", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

35. michaelmina_lab

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("michaelmina_lab", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

36. mlipsitch

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("mlipsitch", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

37. mugecevik

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("mugecevik", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

38. mvankerkhove

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("mvankerkhove", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

39. nytimes

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("nytimes", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

40. NYUDocs

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("NYUDocs", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

41. profvrr

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("profvrr", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

42. projecthopeorg

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("projecthopeorg", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

43. PublicHealth

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("PublicHealth", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

44. RoopaDhatt

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("RoopaDhatt", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

45. SavetheChildren

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("SavetheChildren", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

46. SCBriand

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("SCBriand", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

47. ScottGottliebMD

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("ScottGottliebMD", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

48. SteveFDA

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("SteveFDA", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

49. T_Inglesby

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("T_Inglesby", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

50. trishgreenhalgh

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("trishgreenhalgh", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

51. trvrb

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("trvrb", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

52. US_FDA

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("US_FDA", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

53. UWVirology

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("UWVirology", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

54. vivek_murthy

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("vivek_murthy", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```

55. WHO

```{r}
twitter_token <- create_token(app = "Miladn5", consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" , consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi", set_renv = TRUE)
MILAD_tweets <- get_timeline("WHO", n= 6400)
n0=count(MILAD_tweets)
MILAD_tweets_FC <- MILAD_tweets %>% arrange(-favorite_count)%>% select(favorite_count)
MILAD_tweets_FC 
MILAD_tweets_RC <- MILAD_tweets %>% arrange(-retweet_count)%>% select(retweet_count) 
MILAD_tweets_RC   
summary(MILAD_tweets)
MILAD_tweets_organic <- MILAD_tweets[MILAD_tweets$is_retweet==FALSE, ] # Remove retweets
MILAD_tweets_organic <- subset(MILAD_tweets_organic, is.na(MILAD_tweets_organic$reply_to_status_id))# Remove replies
MILAD_tweets_organic_FC <- MILAD_tweets_organic %>% arrange(-favorite_count)%>% select(favorite_count)  
print(MILAD_tweets_organic_FC)
MILAD_tweets_organic_RC <- MILAD_tweets_organic %>% arrange(-retweet_count)%>% select(retweet_count)  
MILAD_tweets_organic_RC
summary(MILAD_tweets_organic)
n1=count(MILAD_tweets_organic)
MILAD_retweets <- MILAD_tweets[MILAD_tweets$is_retweet==TRUE,]# Keeping only the retweets
n2=count(MILAD_retweets)
MILAD_replies <- subset(MILAD_tweets, !is.na(MILAD_tweets$reply_to_status_id))# Keeping only the replies
n3=count(MILAD_replies) 
c(n0,n1,n2,n3)
data$fraction = data$count / sum(data$count)# Adding columns 
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Type_of_Tweet <- paste(data$category, data$percentage, "%") # Specify what the legend should say
MILAD_app <- MILAD_tweets %>% select(source) %>%  group_by(source) %>% summarize(count=n())
MILAD_app <- subset(MILAD_app, count > 11)
print(MILAD_app)
data <- data.frame( category=MILAD_app$source,count=MILAD_app$count)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
Source <- paste(data$category, data$percentage, "%")
MILAD_tweets_organic$text <-  gsub("https\\S*", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text <-  gsub("@\\S*", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("amp", "", MILAD_tweets_organic$text) 
MILAD_tweets_organic$text  <-  gsub("[\r\n]", "", MILAD_tweets_organic$text)
MILAD_tweets_organic$text  <-  gsub("[[:punct:]]", "", MILAD_tweets_organic$text)
tweets <- MILAD_tweets_organic %>%select(text) %>% unnest_tokens(word, text)
tweets <- tweets %>% anti_join(stop_words)
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +labs(y = "Count",x = "Unique words", title = "Most frequent words found in the tweets of MILAD", subtitle = "Stop words removed from the list")
tweets %>% count(word, sort = TRUE) %>% top_n(10)
MILAD_tweets_organic$hashtags <- as.character(MILAD_tweets_organic$hashtags)
MILAD_tweets_organic$hashtags <- gsub("c\\(", "", MILAD_tweets_organic$hashtags)   
set.seed(1234)
wordcloud(MILAD_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))  
set.seed(1234)
wordcloud(MILAD_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,  colors=brewer.pal(8, "Dark2"))
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")# Converting tweets to ASCII to trackle strange characters
 tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)# removing retweets, in case needed
tweets <-gsub("@\\w+","",tweets)# removing mentions, in case needed
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
sentimentscores
```


