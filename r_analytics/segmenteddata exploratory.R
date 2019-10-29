######### LOADING LIBRARY ######### 
#library("rjson") -- not working
library("jsonlite")
library("tm")
#library("rJava")
library("wordcloud")
#library("textir")
#library("RWeka")
#library("qdap")
#library("maptpx")
library("syuzhet")
library("topicmodels")
library("tidytext")

######### BEGIN EXPLORE ######### 
articles <- fromJSON("~/nlp_challege/datasets/articles.json")
head(articles)
str(articles)


barplot(table(articles$sentiment,articles$source$country),las=2)
hist(articles$sentiment[articles$country=="United States"])


us_only <- articles[articles$country=="United States",]

######### NLP ######### 
#### BUILDING CORPUS ####
contentCorpus<- iconv(articles$contents,to="utf-8-mac")
ccorpus<-Corpus(VectorSource(contentCorpus))

excerptCorpus<- iconv(articles$excerpt,to="utf-8-mac")
ecorpus<-Corpus(VectorSource(excerptCorpus))

inspect(ccorpus[1:5])

#### CLEANING ####
removeURL<- function(x) gsub('http[[:alnum:]]*','',x)
changeWords<- function(x) gsub('australi','',x)

ecorpus<-tm_map(ecorpus,tolower) #make it all lowercase
ecorpus<-tm_map(ecorpus,removePunctuation)
ecorpus<-tm_map(ecorpus,removeNumbers)
stopwords("english")
ecleaned<-tm_map(ecorpus,removeWords,stopwords("english"))
ecleaned<-tm_map(ecorpus,content_transformer(changeWords))
inspect(ecleaned[1:5])
#remove united and airlines
ecleaned<-tm_map(ecleaned,removeWords,c(stopwords("english"),'plane',"united","airlines","flight","airline","passenger","flights","will","according","…","—","’s","says","said"))
inspect(ecleaned[1:5])
ecleaned<-tm_map(ecleaned,stripWhitespace)


#### k1 ####
k1 <- read.csv("~/nlp_challege/four-fold-export/k1.csv")
contentCorpus<- iconv(k1$contents,to="utf-8-mac")
ccorpus<-Corpus(VectorSource(contentCorpus))

ccorpus<-tm_map(ccorpus,tolower) 
ccorpus<-tm_map(ccorpus,removePunctuation)
ccorpus<-tm_map(ccorpus,removeNumbers)
ccleaned<-tm_map(ccorpus,removeWords,c(stopwords("en"),'\\n','plane',"united","airlines","flight","airline","passenger","flights","will","according","…","—","’s","says","said"))
ccleaned<-tm_map(ccorpus,removeWords,c('\\n'))
inspect(ccleaned[1:5])
ccleaned<-tm_map(ccleaned,stripWhitespace)
ctdm<-TermDocumentMatrix(ccleaned)
cmatrix<-as.matrix(ctdm)

cmatrix[1:10,1:20]

attributes(ccleaned)
df <- data.frame(text = get("content", ccleaned),stringsAsFactors = F)
head(df$text)
str(df)
sentiment <- get_nrc_sentiment(df$text)
df$s<-sentiment
head(sentiment)
head(df$s)
#### TERM DOCUMENT MATRIX ####
#to structure the data
etdm<-TermDocumentMatrix(ecleaned)
etdm

ematrix<-as.matrix(etdm)
ematrix[1:10,1:20]
ematrix<-NULL
#### BARPLOT #### 
w <- rowSums(cmatrix)
sw[1:10]
sw<-sort(w,TRUE)
ov2k <- w[w>1000]
barplot(sw[1:100],las=2,main="Over 2000")
sw[1:100]
w["new"]


##### SENTIMENT AGAINST PUBLISHER ####
#FREQUENCY
par(mfrow=c(1,1))
barplot(table(articles$sentiment[is.element(articles$source$publisher,names(tpub[1:10]))],articles$source$publisher[is.element(articles$source$publisher,names(tpub[1:10]))]),
        legend= T,
        ylim=c(0,1300), col = c("seagreen1", "seagreen3", "seagreen4"), 
        main = "Sentiment Analysis Against Top 10 Publishers",
        xlab= "Publishers",
        ylab = "Number of articles",
        cex.main = 3,
        cex.axis = 1.5,
        cex.lab = 1.5)
contingencytb<-table(articles$sentiment[is.element(articles$source$publisher,names(tpub[1:10]))],articles$source$publisher[is.element(articles$source$publisher,names(tpub[1:10]))])
for(i in 1:10){
  contingencytb[,i]<-contingencytb[,i]/sum(contingencytb[,i])
}
barplot(contingencytb,   
        legend = T,
        col = c("seagreen1", "seagreen3", "seagreen4"),
        main = "Proportion of Sentiment Analysis Against Top 10 Publishers",
        cex.main =3,
        xlab= "Publishers",
        cex.axis = 1.5,
        cex.lab = 1.5,
        ylab = "Frequency of number of articles",
        ylim = c(0,1.35)) # PROPORTION OF THE SENTIMENTS
?cex
contingencytb
par(bg = "black")
par(bg = "white")

#### WORDCLOUD #### 
set.seed(222)
wordcloud(words=names(sw),
          freq=sw,
          max.words = 100,
          min.freq = 2000,
          colors = brewer.pal(8,"Dark2"))

#### SENTIMENT ANALYSIS #### 
attributes(ecleaned)
df <- data.frame(text = get("content", ecleaned),stringsAsFactors = F)
head(df$text)
str(df)
sentiment <- get_nrc_sentiment(df$text)
df$s<-sentiment
head(sentiment)
head(df$s)

typeof(sentiment)
barplot(table(sentiment))
table(sentiment)

names(sentiment)
barplot(table(sentiment))
barplot(table(as.vector(sentiment)))
tableofsentiment<-c(sum(sentiment$anger),
                    sum(sentiment$anticipation),
                    sum(sentiment$disgust),
                    sum(sentiment$fear),
                    sum(sentiment$joy),
                    sum(sentiment$sadness),
                    sum(sentiment$surprise),
                    sum(sentiment$trust),
                    sum(sentiment$negative),
                    sum(sentiment$positive))

barplot(tableofsentiment,
        names.arg = names(sentiment))


negsen<-c(sum(sentiment$anger),
          sum(sentiment$disgust),
          sum(sentiment$fear),
          sum(sentiment$sadness),
          sum(sentiment$surprise))

barplot(negsen,
        names.arg = c("Anger","Disgust","Fear","Sadness","Surprise"),
        main = "Frequency of Negative Words for Sentiment Analysis",
        xlab = "Negative words",
        ylab = "Frequency of the negative words",
        col = "seagreen2",
        ylim = c(0,30000))

#### EXPORTING CSV ####
#OK HAVE TO DROP AUTHORS BECAUSE IT IS A LIST OF CHR AND NOT A VECTOR SO CANNOT USE WRITE WITH IT--
selected <- articles[,!names(articles) %in% c("authors","has_video","topics","image_link","link","source.link")]
selected <- flatten(selected)

first <- sample(1:39109,9777)
k1<-selected[first,]
selected<-selected[-first,]

second <- sample(1:29332,9777)
k2<-selected[second,]
selected<-selected[-second,]

third <- sample(1:19555,9777)
k3<-selected[third,]

k4<-selected[-third,]

write.csv(k1,"../four-fold-export/k1.csv",row.names = F)
write.csv(k2,"../four-fold-export/k2.csv",row.names = F)
write.csv(k3,"../four-fold-export/k3.csv",row.names = F)
write.csv(k4,"../four-fold-export/k4.csv",row.names = F)

?write.csv
head(k1)

#### TOP10PUBLISHERS ####
test1<-read.csv("../four-fold-export/k1.csv")
#head(test1)
tpub <- table(articles$source$publisher)
tpub<- sort(tpub,T)
barplot(tpub[1:10],las=1,
        main="Top 10 Publishing Sources",
        ylab="Amount of Articles",
        xlab="Sources",
        names.arg = names(tpub[1:10]),
        col="red")
names(tpub[1:10])

#### TOPIC CLUSTERING ####

# set a seed so that the output of the model is predictable
k =4
etdm_lda <- LDA(etdm, k = k, control = list(seed = 1234))
etdm_lda

ldamat<-as.matrix(topics(etdm_lda))
which(ldamat[,1]==2)[1:20]
ldamat[30]

etdm_topics <- tidy(ldamat,matrix="beta")
etdm_topics[100:150,]

#### EXPLORING SEGMENTED DATA FINAL ####
segmented <- read.csv("~/Desktop/nlp_challenge/segmented_data_final.csv", stringsAsFactors=FALSE)
View(segmented)

articles1 <- read.csv("~/Desktop/nlp_challenge/articles.csv", stringsAsFactors=FALSE)
View(articles1)

# How many articles in total #
nrow(segmented)
# Answer: 39109

# How many unique articles #
#ua <- table(articles1$link)
#uadataframe <- as.data.frame(ua)
#nrow(uadataframe)
# Answer 39109
ua1 <- table(segmented$contents)
uadataframe1 <- as.data.frame(ua1)
nrow(uadataframe1)
# Answer: 32200 - correct one

# How many publishers
unique(segmented$source.publisher)
totalpublisher <- unique(segmented$source.publisher)
# Total: 3763 publishers
tp <- table(segmented$source.publisher)
tpdataframe <- as.data.frame(tp)
nrow(tpdataframe)
# Total: 3763 publishers

# What is the most frequent country
barplot(table(articles$sentiment,articles$source$country),las=2)
barplot(table(segmented$source.country), col = "mediumseagreen",
        main = "Country of Origin of Published Articles", cex.main = 4,
        ylab = "Number of Articles",
        cex.axis = 1,
        cex.lab = 1.5,
        mgp = c(3,0.5,0),
        ylim = c(0,30000),
        las = 2,
        mai = c(5,4,4,2))
par(xpd=TRUE)
title(xlab = "Country of Origin", line = 4)
par(xpd=FALSE)
par(mar=c(5, 4, 4, 2) + 0.1)
par(mai=c(4,3,2,1))
?par
dev.off()
table(segmented$source.ountry)
# Answer: United States - 28247 articles

# What is the most frequent publishers?
tpub <- table(articles$source$publisher)
tpub<- sort(tpub,T)
names(tpub[1:10])
# Answer: Yahoo.com 
tpdataframe <- as.data.frame(tp)
# Answer: Yahoo.com has 964 articles

# What are the proportion 1,0,-1 sentiments provided in the articles JSON dataset - percentage wise 
table(articles$sentiment)
barplot(table(articles$sentiment))
prop.table(table(articles$sentiment)) *100
# Answer: Proportion is:
# -1          0          1 
# 24.2425017 74.7705132  0.9869851 
barplot(prop.table(table(articles$sentiment))*100, col = "seagreen3",
        ylim = c(0,100),
        main = "Proportion of -1,0,1 sentiments in the articles",
        xlab = "Sentiment",
        ylab = "Frequency of sentiment")

# What is the mean of the fb_data.total_shares #
mean(articles1$fb_data.shares)
# 12.88742

# What is the mean of the tw_data.total_engagement #
mean(articles1$tw_data.tw_count)
# 20.07551 - but this is twitter count not total engagement

# What is the range of fb_data.total_shares, total_engagement_count, tw_data.tw_Count. #
range(articles1$fb_data.shares)
# 0 - 68203
range(articles1$fb_data.total_engagement_count)
# 0 - 660561
range(articles1$tw_data.tw_count)
# 0 - 30913

# What is the top 25%, top 75% of the FB total_engagement_count? #
quantile(articles1$fb_data.total_engagement_count, c(.25, 0.75), na.rm=T)
# Top 25%: 0 engagement, Top 75% = 60 engagement

# What is the top 75% of the tw_data.tw_count? #
quantile(articles1$tw_data.tw_count, c(.25, 0.75), na.rm=T)
# Top 25%: 0 engagement, top 75%= 4 engagement

# Top 10 Fastest Rising Publishers by Velocity
# Highest Velocity of a single article
rart <- segmented[segmented$velocity > 33,]
rart1 <- rart[order(rart[,5],decreasing=TRUE),]
barplot(rart1[,5], names.arg=rart1[,4],
        main = "Top 10 Fastest Rising Publishers with by Highest Velocity of A Single Article",
        cex.main = 3,
        cex.axis = 1.5,
        cex.lab = 1.5,
        col = "mediumseagreen",
        ylim = c(0,300),
        ylab = "Velocity",
        xlab = "Source publisher")
# Each publishers, add up the total velocity
tvel <- aggregate(velocity ~ source.publisher, data=segmented, sum)
tvel1 <- tvel[tvel$velocity > 80,]
barplot(tvel1$velocity ~ tvel1$source.publisher)
tvel3 <- tvel1[order(tvel1[,2],decreasing=TRUE),]
barplot(tvel3[,2],names.arg=tvel3[,1],
        main = "Top 10 Fastest Rising Publishers by Highest Aggregate Velocity",
        cex.main = 3,
        cex.axis = 1.5,
        cex.lab = 1.5,
        ylim = c(0,300),
        xlab = "Source publisher",
        ylab = "Total or aggregate velocity",
        col = "mediumseagreen")
# For the top 10 publishers, distribution of velocity
yahoo <- segmented[segmented$source.publisher == "yahoo.com",]
thevillagessuntimes.com <- segmented[segmented$source.publisher == "thevillagessuntimes.com",]

par(mfrow=c(2,5))
par(mfrow=c(1,1))
hist(segmented[segmented$source.publisher == "yahoo.com",]$velocity,
     main = "Distribution of Velocity of Yahoo's Articles",
     col = "palegreen2",
     xlab = "Velocity")
hist(segmented[segmented$source.publisher == "cbsnews.com",]$velocity,
     main = "Distribution of Velocity of CBS News' Articles",
     col = "palegreen2",
     xlab = "Velocity")
hist(segmented[segmented$source.publisher == "boardingarea.com",]$velocity,
     main = "Distribution of Velocity of Boarding Area's Articles",
     col = "palegreen2",
     xlab = "Velocity")
hist(segmented[segmented$source.publisher == "nbc.com",]$velocity,
     main = "Distribution of Velocity of NBC's Articles",
     col = "palegreen2",
     xlab = "Velocity")
hist(segmented[segmented$source.publisher == "foxnews.com",]$velocity,
     main = "Distribution of Velocity of Boarding Area's Articles",
     col = "palegreen2",
     xlab = "Velocity")
hist(segmented[segmented$source.publisher == "flyertalk.com",]$velocity,
     main = "Distribution of Velocity of Flyer Talk's Articles",
     col = "palegreen2",
     xlab = "Velocity")
hist(segmented[segmented$source.publisher == "aviationpros.com",]$velocity,
     main = "Distribution of Velocity of Aviation Pros' Articles",
     col = "palegreen2",
     xlab = "Velocity")
hist(segmented[segmented$source.publisher == "4-traders.com",]$velocity,
     main = "Distribution of Velocity of 4-traders' Articles",
     col = "palegreen2",
     xlab = "Velocity")
hist(segmented[segmented$source.publisher == "sfgate.com",]$velocity,
     main = "Distribution of Velocity of SF Gate's Articles",
     col = "palegreen2",
     xlab = "Velocity")
hist(segmented[segmented$source.publisher == "thevillagessuntimes.com",]$velocity,
     main = "Distribution of Velocity of The Villages Suntimes' Articles",
     col = "palegreen2",
     xlab = "Velocity")

par(mfrow=c(2,5))
hist(log(segmented[segmented$source.publisher == "yahoo.com",]$velocity,10), 
     main = "Distribution of Velocity of Yahoo's Articles",
     col = "palegreen2",
     ylim = c(0,200),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "cbsnews.com",]$velocity,10),
     main = "Distribution of Velocity of CBS News' Articles",
     col = "palegreen2",
     ylim = c(0,250),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "boardingarea.com",]$velocity,10),
     main = "Distribution of Velocity of Boarding Area's Articles",
     col = "palegreen2",
     ylim = c(0,200),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "nbc.com",]$velocity,10),
     main = "Distribution of Velocity of NBC's Articles",
     col = "palegreen2",
     ylim = c(0,150),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "foxnews.com",]$velocity,10),
     main = "Distribution of Velocity of Fox News' Articles",
     col = "palegreen2",
     ylim = c(0,150),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "flyertalk.com",]$velocity,10),
     main = "Distribution of Velocity of Flyer Talk's Articles",
     col = "palegreen2",
     ylim = c(0,50),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "aviationpros.com",]$velocity,10),
     main = "Distribution of Velocity of Aviation Pros' Articles",
     col = "palegreen2",
     ylim = c(0,50),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "4-traders.com",]$velocity,10),
     main = "Distribution of Velocity of 4-Traders' Articles",
     col = "palegreen2",
     ylim = c(0,2),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "sfgate.com",]$velocity,10),
     main = "Distribution of Velocity of Sf Gate's Articles",
     col = "palegreen2",
     ylim = c(0,60),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "thevillagessuntimes.com",]$velocity,10)) # doesnt work bcs it's non existent

par(mfrow=c(2,5))
hist(log(segmented[segmented$source.publisher == "yahoo.com",]$velocity,10), 
     main = "Distribution of Velocity of Yahoo's Articles",
     col = "palegreen2",
     ylim = c(0,200),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "cbsnews.com",]$velocity,10),
     main = "Distribution of Velocity of CBS News' Articles",
     col = "palegreen2",
     ylim = c(0,200),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "boardingarea.com",]$velocity,10),
     main = "Distribution of Velocity of Boarding Area's Articles",
     col = "palegreen2",
     ylim = c(0,200),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "nbc.com",]$velocity,10),
     main = "Distribution of Velocity of NBC's Articles",
     col = "palegreen2",
     ylim = c(0,200),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "foxnews.com",]$velocity,10),
     main = "Distribution of Velocity of Fox News' Articles",
     col = "palegreen2",
     ylim = c(0,200),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "flyertalk.com",]$velocity,10),
     main = "Distribution of Velocity of Flyer Talk's Articles",
     col = "palegreen2",
     ylim = c(0,200),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "aviationpros.com",]$velocity,10),
     main = "Distribution of Velocity of Aviation Pros' Articles",
     col = "palegreen2",
     ylim = c(0,200),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "4-traders.com",]$velocity,10),
     main = "Distribution of Velocity of 4-Traders' Articles",
     col = "palegreen2",
     ylim = c(0,200),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "sfgate.com",]$velocity,10),
     main = "Distribution of Velocity of Sf Gate's Articles",
     col = "palegreen2",
     ylim = c(0,200),
     xlab = "Re-expressed velocity")
hist(log(segmented[segmented$source.publisher == "thevillagessuntimes.com",]$velocity,10)) # doesnt work bcs it's non existent





