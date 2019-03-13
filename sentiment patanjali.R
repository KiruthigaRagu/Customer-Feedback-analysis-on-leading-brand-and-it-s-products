library("SnowballC")
library("tm")
library("twitteR")
library("syuzhet")

data<-read.csv("Contact Information.csv")
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

#REMOVING STOP WORDS

tweets<-removeWords(tweets, stopwords('en'))

#STEMMING

tweets<-stemDocument(tweets)

	
#WORDS AND THEIR FREQUENCIES

s<- tweets.df2
ss <- data.frame(x=unlist( str_split(s, " ")))

sss <- setDT(ss)[, .(freq = .N), x]
freq <- as.data.frame(sss)

#EMOTIONS

word.df <- as.vector(tweets)
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
data$feedback

data$Class
write.csv(data,"D:/Contact1.csv")
head(data)


