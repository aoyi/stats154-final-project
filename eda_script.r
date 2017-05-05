# load datasest 
user = read.csv("/Users/ashley_mou/Documents/Stat 154/project/yelp_academic_dataset_user.csv")
review.train = read.csv("/Users/ashley_mou/Documents/Stat 154/project/yelp_academic_dataset_review_train.csv")
business.train = read.csv("yelp_academic_dataset_business_train.csv")

### EDA Summary Plot ###

library(formattable)
#info of user
ur <- user$review_count
r.10 = percent(length(ur[ur < 10]) / length(ur))
r.100 = percent(length(ur[ur > 100]) / length(ur))

us = user$average_stars
avg.2 = percent(length(us[us<=2]) / length(us))
avg.4.5 = percent(length(us[us>=4.5]) / length(us))

# Review count
review = as.vector(business.train$review_count)
barplot(sort(review))
r1 = length(review[review <11])
r2 = length(review[review > 10 & review < 21])
r3 = length(review[review > 20 & review < 31])
r4 = length(review[review > 30 & review < 41])
r5 = length(review[review > 40 & review < 51])
r6 = length(review[review > 50 & review < 61])
r7 = length(review[review > 60 & review < 71])
r8 = length(review[review > 70 & review < 81])
r9 = length(review[review > 80])
review_c = c(r1,r2,r3,r4,r5,r6,r7,r8,r9)
lbls = c("<10","[11,20]","[21,30]","[31,40]","[41,50]","[51,60]","[61,70]","[71,80]",">80")
bar = barplot(review_c, col = c("black","red"), names.arg = lbls, xlab = "Review received", ylab = "Number of restaurant",main = "Fig1:Number of review a restaurant received")

# Rating count
rate = as.vector(business.train$stars)
rate1 = length(rate[rate == 1])
rate2 = length(rate[rate == 1.5])+length(rate[rate == 2])
rate3 = length(rate[rate == 2.5])+length(rate[rate == 3])
rate4 = length(rate[rate == 3.5])+length(rate[rate == 4])
rate5 = length(rate[rate == 4.5])+length(rate[rate == 5])
rate_c = c(rate1,rate2,rate3,rate4,rate5)
pie(rate_c,labels = c("1","1.5-2","2.5-3","3.5-4","4.5-5"),main = "Fig2:Distribution of Restaurant rating")

### WordCloud ###

# load tm package for text mining 
library(tm)

# create a function for text cleaning
clean.text = function(corpus){
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords, stopwords())
  return(corpus)
}

#extract 5-stars business and 1-star business
library(dplyr)
stars5_review.train = review.train %>% filter(stars==5)
stars1_review.train = review.train %>% filter(stars==1)
stars2_review.train = review.train %>% filter(stars==2)

# create a metadata
complete.text = readTabular(
  mapping = list(content = "text", 
                 user = "user_id"
               )
)

# Make a corpus
stars5.text.corpus = Corpus(
  DataframeSource(stars5_review.train), 
  readerControl = list(reader = complete.text)
)

stars1.text.corpus = Corpus(
  DataframeSource(stars1_review.train), 
  readerControl = list(reader = complete.text)
)

# Clean text
star5.text.corpus.clean = clean.text(stars5.text.corpus)
star1.text.corpus.clean = clean.text(stars1.text.corpus)

#convert it into DTM
library(cyphid)

star5.text.dtm = DocumentTermMatrix(star5.text.corpus.clean)
star5.text = removeSparseTerms(star5.text.dtm, 0.99)

star1.text.dtm = DocumentTermMatrix(star1.text.corpus.clean)
star1.text = removeSparseTerms(star1.text.dtm, 0.99)

#create word cloud
library(wordcloud)

star5.freq = data.frame(sort(colSums(as.matrix(star5.text)), decreasing = TRUE))
star5.cloud = wordcloud(rownames(star5.freq), star5.freq[,1], max.words = 100, colors = brewer.pal(1, "Dark2"))
star5.cloud

star1.freq = data.frame(sort(colSums(as.matrix(star1.text)), decreasing = TRUE))
star1.cloud = wordcloud(rownames(star1.freq), star1.freq[,1], max.words = 100, colors = brewer.pal(1, "Dark2"))
star1.cloud



