library(dplyr)
library(stringr)
library(stringdist)
library(data.table)

library(caret)
library(caTools)
library(e1071)

library(rpart)
library(rpart.plot)
library(neuralnet)
library(party)

library("SnowballC")
library("tm")
library("twitteR")
library("syuzhet")

data<-read.csv("D:/Ml project.csv")
tweets.df<-data$feedback

# PUNCTUATION REMOVAL

tweets.df2 <- gsub("http.*","",tweets.df)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)
tweets.df2 <- gsub("http.*","",tweets.df2)
tweets.df2 <- gsub("\n","",tweets.df2)
tweets.df2 <- gsub("(","",tweets.df2)
tweets.df2 <- gsub(")","",tweets.df2)
tweets.df2 <- gsub("-","",tweets.df2)
tweets.df2 <- gsub("_","",tweets.df2)
tweets.df2 <- gsub("{","",tweets.df2)
tweets.df2 <- gsub("}","",tweets.df2)
tweets.df2 <- gsub("]","",tweets.df2)
tweets.df2 <- gsub("[","",tweets.df2)
tweets.df2 <- gsub("\","",tweets.df2)
tweets.df2 <- gsub("/","",tweets.df2)
tweets.df2 <- gsub("+","",tweets.df2)
tweets.df2 <- gsub("&","",tweets.df2)
tweets.df2 <- gsub("?","",tweets.df2)
tweets.df2 <- gsub("'","",tweets.df2)
tweets.df2 <- gsub(""","",tweets.df2)
tweets.df2 <- gsub("<","",tweets.df2)
tweets.df2 <- gsub(">","",tweets.df2)
tweets.df2 <- gsub("%","",tweets.df2)
tweets.df2 <- gsub("!","",tweets.df2)
tweets.df2 <- gsub("~","",tweets.df2)
tweets.df2 <- gsub("`","",tweets.df2)

#TRIMMING SPACES

tweets.df2<-str_replace(gsub("\\s+", " ", str_trim(tweets.df2)), "B", "b")
head(tweets.df2)

#CASE CONVERSION

tweets<-lapply(tweets.df2, function(v) {
   if (is.character(v)) return(tolower(v))
   else return(v)
 })
  
#DIGIT EXTRACTION

tweets<-sub("[[:digit:]]+","nuumber",tweets)
	
#WORDS AND THEIR FREQUENCIES

s<- tweets.df2
ss <- data.frame(x=unlist( str_split(s, " ")))

sss <- setDT(ss)[, .(freq = .N), x]
freq <- as.data.frame(sss)

#EMOTIONS

word.df <- as.vector(tweets.df2)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(tweets.df2, emotion.df) 
 
head(emotion.df2)
sent.value <- get_sentiment(word.df)

#BINDING WITH DATA

Class<-sent.value
data<-cbind(data,Class)
data$Class2<-0
data$Class2[data$Class>0]<-1
data$Class2[data$Class<0]<--1
data$Class<-NULL
data$Class<-data$Class2
data$Class2<-NULL
data$feedback<-NULL
write.csv(data,"D:/Contact.csv")
head(data)

samplesize=0.80*nrow(data)
set.seed(99)
index=sample(seq_len(nrow(data)),size=samplesize)
datatrain<-data[index,]
datatest<-data[-index,]

fit<-rpart(as.factor(Class)~used+Opinion.on.price+Product.Size.opinion+Age+Gender+Marital.status+Education+Locality+Beauty.Care+Food.and.beverages+Home.care.products+Medicines+Textile.Products,data=datatrain)
prediction<-predict(fit,datatrain[,names(datatrain)!="Class"],type="class")
confusionMatrix(prediction,as.factor(datatrain$Class))
rpart.plot(fit)

fit<-ctree(as.factor(Class)~used+Opinion.on.price+Product.Size.opinion+Age+Gender+Marital.status+Education+Locality+Beauty.Care+Food.and.beverages+Home.care.products+Medicines+Textile.Products,data=datatrain)
prediction<-predict(fit,datatrain[,names(datatrain)!="Class"])
confusionMatrix(prediction,as.factor(datatrain$Class))
(fit)

