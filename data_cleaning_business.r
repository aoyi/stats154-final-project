##### PART 4: MODEL FITTING: BUSINESS ATTRIBUTES AND RATING ##### 

# Load data
business.test = read.csv("~/Desktop/2017_Spring/STAT154/yelp/data/yelp_academic_dataset_business_test.csv")   #440 x 17
business.train = read.csv("~/Desktop/2017_Spring/STAT154/yelp/data/yelp_academic_dataset_business_train.csv") #2510 x 17
checkin = read.csv("~/Desktop/2017_Spring/STAT154/yelp/data/yelp_academic_dataset_checkin.csv")               #2877 x 4

### Training set data cleaning
# Select most informative columns
cleaned.business = business.train[, c("business_id", "is_open", "review_count")]

# Readd cleaned category to each business
library("tm")
category = business.train$categories
removed.tag = c("Restaurants", "Food")
cleaned.category = removeWords(as.character(business.train$categories), removed.tag)
cleaned.business$category = as.matrix(as.factor(gsub( "[^[:alnum:]]", "", cleaned.category)))

# Add checkin column to each business
library(stringr)
timestring = str_split(checkin$time,",")
countstring = sapply(sapply(timestring, function(x){str_split(x,":")}), function(x){sapply(x, "[[", 2)}) 
counts = sapply(countstring, function(x){str_replace_all(x, c("'" = "" , "]"= ""))})
counts = sapply(counts, as.numeric)
sumcounts = sapply(counts, sum)
checkin$sum = sumcounts

cleaned.business$checkin = rep(0,nrow(business.train))  #add checkin column
for (i in 1:nrow(checkin)){
  if (checkin$business_id[i] %in% cleaned.business$business_id){
    cleaned.business[which(cleaned.business$business_id==toString(checkin$business_id[i])),"checkin"] = checkin$sum[i]
  }
}


### Testing set data cleaning
# Select most informative columns
cleaned.business.test = business.test[, c( "business_id", "is_open", "review_count")]

# Readd cleaned category to each business
library("tm")
category.test = business.test$categories
removed.tag = c("Restaurants", "Food")
cleaned.category.test = removeWords(as.character(business.test$categories), removed.tag)
cleaned.business.test$category = as.matrix(as.factor(gsub( "[^[:alnum:]]", "", cleaned.category.test)))

# Add checkin column to each business
cleaned.business.test$checkin = rep(0,nrow(cleaned.business.test))  #add checkin column
for (i in 1:nrow(checkin)){
  if (checkin$business_id[i] %in% cleaned.business.test$business_id){
    cleaned.business.test[which(cleaned.business.test$business_id==toString(checkin$business_id[i])),"checkin"] = checkin$sum[i]
  }
}

### Add selected attributes to both business.train and business.test
business.allattr = read.csv("~/Desktop/2017_Spring/STAT154/yelp/data/yelp_business_added_attributed_all.csv"
                            ,na.strings=c("", "NA")) #20-71 are attributes
# select attributes
allattr = business.allattr[,-c(1:19)] #2950 business x 52 attr
na.attr = apply(allattr, 2, function(x){sum(is.na(x))})
which(na.attr>nrow(allattr)/4) #which attributes that has more than 1/4 NA values
subattr = allattr[,-which(na.attr>nrow(allattr)/4)] #delet those attribut

business.train.subattr = cbind(cleaned.business, subattr[c(1:nrow(cleaned.business)),]) #2510x15 mat
business.test.subattr = cbind(cleaned.business.test,
                              subattr[c(2511:nrow(subattr)),]) #440x15 mat

#write.csv(business.train.subattr, file = "cleaned_businesstrain_subattr.csv")
#write.csv(business.test.subattr, file = "cleaned_businesstest_subattr.csv")

# delete rows that has too many NA values
business.train.subattr = cbind(business.train.subattr, business.train$stars)   # add rating to training 2510  x 15
colnames(business.train.subattr)[ncol(business.train.subattr)] = "rating"
NAattr = apply(business.train.subattr[,-c(1:5,15)],1, function(x){sum(is.na(x))})  #10 attr
business.train.sub = business.train.subattr[-which(NAattr>5) ,]  #delete rows that has more than 5 na.values
business.train.sub = as.matrix(business.train.sub) # 2297   15
business.train.sub[is.na(business.train.sub)] = 0 #replace rest NA value to 0
business.train.sub = data.frame(business.train.sub) 
business.train.sub = business.train.sub[,-1] # delete business_id  2297 x 14

business.train.sub$category = as.numeric(business.train.sub$category)
for (i in c(1:14)[-3]){
  business.train.sub[,i] = as.numeric(as.character(business.train.sub[,i]))
}
dim(business.train.sub)
colnames(business.train.sub)

#### Add review prediction as new variable
library(tidyverse)
review.train=read.csv("~/Desktop/2017_Spring/STAT154/yelp/data/yelp_academic_dataset_review_train.csv") #116474
review.test = read.csv("~/Desktop/2017_Spring/STAT154/yelp/data/yelp_academic_dataset_review_test.csv")
boost.pred.review = read.csv("~/Desktop/2017_Spring/STAT154/yelp/data/gbr_d15_train_pred_1.csv", header = FALSE)
review.pred = data.frame(business_id = review.train$business_id, rating = boost.pred.review )
colnames(review.pred)[2] = "rating"

review.pred.train = rep(NA, nrow(business.train))
for (i in 1:nrow(business.train)) {
  review.pred.train[i] = mean(review.pred$rating[which(review.pred$business_id ==
                                                         as.character(business.train$business_id[i]))])}

review.pred.train = review.pred.train[-which(NAattr>5)] 
business.train.sub$review_pred = review.pred.train
business.train.sub = business.train.sub[,c(1:13,15,14)]
dim(business.train.sub)
colnames(business.train.sub)


