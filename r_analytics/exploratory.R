######### INSTALLING JSON READING ######### 
install.packages("jsonlite")

#########INSTALLING LIBRARIES FOR NLP ######### 
install.packages("tm")
#install.packages("rJava") #FIX macOS SUPPORTING ISSUE BY RUNNING "sudo R CMD javareconf" in TERMINAL
install.packages("wordcloud")
#install.packages("textir")
#install.packages("RWeka")
#install.packages("qdap")
#install.packages("maptpx")
install.packages("syuzhet")
install.packages("topicmodels")
install.packages("tidytext")
install.packages("rpart.plot")
install.packages("msm")
install.packages("sandwich")
install.packages("jtools")#you may be asked to install 'broom' and 'ggstance' packages as well
install.packages("broom")
install.packages("ggstance")

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
library("ggplot2")
library("dplyr")
library("rpart.plot")
library("msm")
library("sandwich")
library("jtools")

######### BEGIN EXPLORE ######### 
articles <- fromJSON("../datasets/articles.json")
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
#remove commonly used words in the articles that have no meaningful contribution
ecleaned<-tm_map(ecleaned,removeWords,c(stopwords("english"),'plane',"united","airlines","flight","airline","passenger","flights","will","according","…","—","’s","says","said"))
inspect(ecleaned[1:5])
ecleaned<-tm_map(ecleaned,stripWhitespace)



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
barplot(table(articles$sentiment[is.element(articles$source$publisher,names(tpub[1:10]))],articles$source$publisher[is.element(articles$source$publisher,names(tpub[1:10]))]),
        legend=T,
        ylim=c(0,1300))
contingencytb<-table(articles$sentiment[is.element(articles$source$publisher,names(tpub[1:10]))],articles$source$publisher[is.element(articles$source$publisher,names(tpub[1:10]))])
for(i in 1:10){
  contingencytb[,i]<-contingencytb[,i]/sum(contingencytb[,i])
}
barplot(contingencytb) # PROPORTION OF THE SENTIMENTS
contingencytb


all_art<- table(articles$source$publisher,articles$sentiment)

for(i in 1:3763){
  if(sum(all_art[i,])<10){
    t[]
  }
  t[i,]<-all_art[i,]/sum(all_art[i,])
}
all_art<-sort(all_art,T)
t<-sort(all_art[,"-1"],T)
head(t)
all_art[-1,]
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
          sum(sentiment$surprise),
          sum(sentiment$negative))

barplot(negsen,
        names.arg = c("Anger","Disgust","Fear","Sadness","Surprise","Negative"))

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
k =6
ctdm_lda <- LDA(ctdm,method="Gibbs", k = k, control = list(seed = 9161,iter = 500, verbose = 25))
ctdm_lda


ldamat<-as.matrix(topics(ctdm_lda))
which(ldamat[,1]==2)[1:20]
ldamat

etdm_topics <- tidy(ctdm_lda,matrix="beta")
terms(ctdm_lda)
etdm_topics

tmResult <- posterior(ctdm_lda)
attributes(tmResult)
ncol(ctdm)
beta <- tmResult$terms 
dim(beta)
rowSums(beta)
nrow(ctdm)
theta <- tmResult$topics 
theta
names<-attr(ldamat,"dimnames")

top_terms <- etdm_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms$term<-names[numterm]
length(top_terms$term)
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

numterm<-as.numeric(top_terms$term)
length(numterm)
topic6<-names[numterm[51:60]]
names<-names[[1]]


#### MODEL BUILDING--DECISION TREE ####
features1<-data.frame(excerpt=k1$excerpt,
                      headline=k1$headline,
                      sentiment=k1$sentiment,
                      source.publisher=k1$source.publisher,
                      max_velocity=k1$max_velocity)
fit1 <- rpart(
  max_velocity ~ .,
  data = features1,
  method = "class",
  #control=rpart.control(minsplit=20, cp = .01),
  parms=list(split='information')
)
rpart.plot(fit1)

data(ptitanic)
k1$z<-log10(k1$max_velocity)
hist(k1$z)
sum(k1$z>0)
summary(k1$z)
sum(k1$max_velocity>0)
features2<-data.frame(anger=sentiment$anger,
                      disgust=sentiment$disgust,
                      fear=sentiment$fear,
                      #sentiment=k1$sentiment,
                      #sadness=sentiment$sadness,
                      #shares=k1$fb_data.shares>1000,
                      #twitter = k1$tw_data.tw_count>1000,
                      max_velocity=k1$max_velocity>0.5)
fit1 <- rpart(
  max_velocity ~ .,
  data = features2,
  method = "class",
  control=rpart.control(minsplit=100, cp = .002),
  parms=list(split='information')
)
rpart.plot(fit1)
feature3<-data.frame(anger=sentiment2$anger,
                     disgust=sentiment2$disgust,
                     fear=sentiment2$fear,
                     #sentiment=k1$sentiment,
                     #sadness=sentiment$sadness,
                     #shares=k1$fb_data.shares>1000,
                     #twitter = k1$tw_data.tw_count>1000,
                     max_velocity=verify_max_velocity)
verify_max_velocity=k2$max_velocity>0.5
pred <- predict(fit1,feature3,type="class")
sum(pred==verify_max_velocity)
## K-FOLD X-VALIDATION ##
k1 <- read.csv("../four-fold-export/k1.csv",stringsAsFactors = F)
k2 <- read.csv("../four-fold-export/k2.csv",stringsAsFactors = F)
k3 <- read.csv("../four-fold-export/k3.csv",stringsAsFactors = F)
k4 <- read.csv("../four-fold-export/k4.csv",stringsAsFactors = F)

kf<-list(k1,k2,k3,k4)

fits<-list()
errors<-c()
feature<-c()

for(i in 1:4){
  test <-data.frame(kf[i])
  train<-data.frame()
  for(j in 1:4){
    if(i==j){
      next
    }
    train<-rbind(data.frame(kf[j]))
  }
  excerptCorpus<- iconv(train$excerpt,to="utf-8-mac")
  ecorpus<-Corpus(VectorSource(excerptCorpus))
  
  ecorpus<-tm_map(ecorpus,tolower) 
  ecorpus<-tm_map(ecorpus,removePunctuation)
  ecorpus<-tm_map(ecorpus,removeNumbers)
  ecleaned<-tm_map(ecorpus,removeWords,c(stopwords("en"),'\\n','plane',"united","airlines","flight","airline","passenger","flights","will","according","…","—","’s","says","said"))

  ecleaned<-tm_map(ecleaned,stripWhitespace)

  df <- data.frame(text = get("content", ecleaned),stringsAsFactors = F)

  sentiment <- get_nrc_sentiment(df$text)
  
  features<-data.frame(anger=sentiment$anger,
                       disgust=sentiment$disgust,
                       fear=sentiment$fear,
                       #sentiment=k1$sentiment,
                       #sadness=sentiment$sadness,
                       #shares=k1$fb_data.shares>1000,
                       #twitter = k1$tw_data.tw_count>1000,
                       max_velocity=train$max_velocity>0.5)
  feature<-c(feature,features)
  fit <- rpart(
    max_velocity ~ .,
    data = features,
    method = "class",
    control=rpart.control(minsplit=100, cp = .002),
    parms=list(split='information')
  )
  fits<-c(fits,fit)
  
  
  excerptCorpus<- iconv(test$excerpt,to="utf-8-mac")
  ecorpus<-Corpus(VectorSource(excerptCorpus))
  
  ecorpus<-tm_map(ecorpus,tolower) 
  ecorpus<-tm_map(ecorpus,removePunctuation)
  ecorpus<-tm_map(ecorpus,removeNumbers)
  ecleaned<-tm_map(ecorpus,removeWords,c(stopwords("en"),'\\n','plane',"united","airlines","flight","airline","passenger","flights","will","according","…","—","’s","says","said"))
  
  ecleaned<-tm_map(ecleaned,stripWhitespace)
  
  df2 <- data.frame(text = get("content", ecleaned),stringsAsFactors = F)

  sentiment2 <- get_nrc_sentiment(df2$text)
  
  testfeature<-data.frame(anger=sentiment2$anger,
                          disgust=sentiment2$disgust,
                          fear=sentiment2$fear,
                          #sentiment=k1$sentiment,
                          #sadness=sentiment$sadness,
                          #shares=k1$fb_data.shares>1000,
                          #twitter = k1$tw_data.tw_count>1000,
                          max_velocity=test$max_velocity>0.5)
  pred <- predict(fit,testfeature,type="class")
  er<-nrow(testfeature)-sum(pred==testfeature$max_velocity)
  errors<-c(errors,er)
}



#### BEST FIT DECISION TREE MODEL####
besttrain<-rbind(k2,k3,k4)
besttest<-k1

excerptCorpus<- iconv(besttrain$excerpt,to="utf-8-mac")
ecorpus<-Corpus(VectorSource(excerptCorpus))

ecorpus<-tm_map(ecorpus,tolower) 
ecorpus<-tm_map(ecorpus,removePunctuation)
ecorpus<-tm_map(ecorpus,removeNumbers)
ecleaned<-tm_map(ecorpus,removeWords,c(stopwords("en"),'\\n','plane',"united","airlines","flight","airline","passenger","flights","will","according","…","—","’s","says","said"))
ecleaned<-tm_map(ecleaned,stripWhitespace)

df <- data.frame(text = get("content", ecleaned),stringsAsFactors = F)

sentiment <- get_nrc_sentiment(df$text)

features<-data.frame(anger=sentiment$anger,
                     disgust=sentiment$disgust,
                     fear=sentiment$fear,
                     #sentiment=k1$sentiment,
                     #sadness=sentiment$sadness,
                     #shares=k1$fb_data.shares>1000,
                     #twitter = k1$tw_data.tw_count>1000,
                     max_velocity=besttrain$max_velocity>0.5)

bestfit <- rpart(
  max_velocity ~ .,
  data = features,
  method = "class",
  control=rpart.control(minsplit=100, cp = .002),
  parms=list(split='information')
)
rpart.plot(bestfit)

excerptCorpus<- iconv(besttest$excerpt,to="utf-8-mac")
ecorpus<-Corpus(VectorSource(excerptCorpus))

ecorpus<-tm_map(ecorpus,tolower) 
ecorpus<-tm_map(ecorpus,removePunctuation)
ecorpus<-tm_map(ecorpus,removeNumbers)
ecleaned<-tm_map(ecorpus,removeWords,c(stopwords("en"),'\\n','plane',"united","airlines","flight","airline","passenger","flights","will","according","…","—","’s","says","said"))
#ccleaned<-tm_map(k1ecorpus,removeWords,c('\\n'))
#inspect(ccleaned[1:5])
ecleaned<-tm_map(ecleaned,stripWhitespace)
#etdm<-TermDocumentMatrix(ecleaned)
#cmatrix<-as.matrix(ctdm)

#cmatrix[1:10,1:20]

#attributes(ccleaned)
df2 <- data.frame(text = get("content", ecleaned),stringsAsFactors = F)
#head(df$text)
#str(df)
sentiment2 <- get_nrc_sentiment(df2$text)

testfeature<-data.frame(anger=sentiment2$anger,
                        disgust=sentiment2$disgust,
                        fear=sentiment2$fear,
                        #sentiment=k1$sentiment,
                        #sadness=sentiment$sadness,
                        #shares=k1$fb_data.shares>1000,
                        #twitter = k1$tw_data.tw_count>1000,
                        max_velocity=besttest$max_velocity>0.5)
pred <- predict(fit,testfeature,type="class")
er<-nrow(testfeature)-sum(pred==testfeature$max_velocity)
er/nrow(testfeature)
sum(pred==testfeature$max_velocity)/nrow(testfeature)

#### CULLED ARTICLES ANALYSIS####
articles_final<-read.csv("../datasets/articles_final.csv",stringsAsFactors = F)

excerptCorpus<- iconv(articles_final$excerpt,to="utf-8-mac")
ecorpus<-Corpus(VectorSource(excerptCorpus))

ecorpus<-tm_map(ecorpus,tolower) 
ecorpus<-tm_map(ecorpus,removePunctuation)
ecorpus<-tm_map(ecorpus,removeNumbers)
ecleaned<-tm_map(ecorpus,removeWords,c(stopwords("en"),'\\n','plane',"united","airlines","flight","airline","passenger","flights","will","according","…","—","’s","says","said"))
#ccleaned<-tm_map(k1ecorpus,removeWords,c('\\n'))
inspect(ecleaned[1:5])
ecleaned<-tm_map(ecleaned,stripWhitespace)

df <- data.frame(text = get("content", ecleaned),stringsAsFactors = F)
#head(df$text)
#str(df)
sentiment <- get_nrc_sentiment(df$text)

features<-data.frame(anger=sentiment$anger,
                     disgust=sentiment$disgust,
                     fear=sentiment$fear,
                     max_velocity=articles_final$max_velocity)

lm<-glm(max_velocity ~ ., family = poisson, data = features)
summary(lm)
plot(max_velocity~.,data=features)
abline(lm)

f<-sample(seq(1,4477,1),replace = F,size = 2238)
train<-articles_final[f,]
test<-articles_final[-f,]




excerptCorpus<- iconv(test$excerpt,to="utf-8-mac")
ecorpus<-Corpus(VectorSource(excerptCorpus))

ecorpus<-tm_map(ecorpus,tolower) 
ecorpus<-tm_map(ecorpus,removePunctuation)
ecorpus<-tm_map(ecorpus,removeNumbers)
ecleaned<-tm_map(ecorpus,removeWords,c(stopwords("en"),'\\n','plane',"united","airlines","flight","airline","passenger","flights","will","according","…","—","’s","says","said"))
#ccleaned<-tm_map(k1ecorpus,removeWords,c('\\n'))
inspect(ecleaned[1:5])
ecleaned<-tm_map(ecleaned,stripWhitespace)

df2 <- data.frame(text = get("content", ecleaned),stringsAsFactors = F)
#head(df$text)
#str(df)
sentiment2 <- get_nrc_sentiment(df2$text)

featurestrain<-data.frame(anger=sentiment1$anger,
                          disgust=sentiment1$disgust,
                          fear=sentiment1$fear,
                          max_velocity=train$max_velocity)

featurestest<-data.frame(anger=sentiment2$anger,
                          disgust=sentiment2$disgust,
                          fear=sentiment2$fear,
                          max_velocity=test$max_velocity)

#BUILDING GENERAL LINEAR REGRESSION MODELS BASED ON POISSON

lm2<-glm(max_velocity~.,family="poisson",data = featurestrain)
featurestest$ybar<-predict(lm2,featurestest)
sqrt(sum((featurestest$ybar-featurestest$max_velocity)**2)/nrow(featurestest))

lm3<-glm(anger~max_velocity,family="poisson",data=featurestrain)
plot(anger~max_velocity,data=featurestrain)
summary(lm3)
featurestest$ybar<-predict(lm3,featurestest,type = "response")
featurestest$ybar[1:8]
featurestest$anger[1:8]
sqrt(sum((featurestest$ybar-featurestest$max_velocity)**2)/nrow(featurestest))

lm4<-glm( disgust ~ max_velocity,family="poisson",data=featurestrain)
summary(lm4)
featurestest$ybar<-predict(lm4,featurestest)
featurestest$ybar[1:8]
featurestest$disgust[1:8]

lm5<-glm(fear~max_velocity,family="poisson",data=featurestrain)
summary(lm5)
featurestrain$sadness<-sentiment1$sadness
lm6<-glm(sadness~max_velocity,family="poisson",data=featurestrain)
summary(lm6)
featurestrain$fear<-sentiment1$fear

summary(lm3)
summary(lm4)
summary(lm5)
summary(lm6)

plot(lm3$residuals~predict(lm3))

lm7<-glm(max_velocity~fear+sadness+disgust+anger,family= poisson(link = "log"),data=featurestrain)
summary(lm7)

plot_summs(lm7, scale = TRUE, exp = TRUE)

get_nrc_sentiment()

