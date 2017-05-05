# Load data set
library(data.table)
library(tidyverse)
setwd("~/Desktop/yelp/")
user = fread("yelp_academic_dataset_user.csv")
review.train=read_csv("yelp_academic_dataset_review_train.csv")
review.test = read_csv("yelp_academic_dataset_review_test.csv")
checkin = read_csv("yelp_academic_dataset_checkin.csv")                      #2877 x 4
business.test = read_csv("yelp_academic_dataset_business_test.csv")
business.train = read_csv("yelp_academic_dataset_business_train.csv")  #2510 x 17
tip = read_csv("yelp_academic_dataset_tip.csv")

# Create business.train dataset
cleaned.business = business.train[, c("name", "business_id", "state", "city", "postal_code", "is_open", "review_count")]

# Add column category to each business 
library("tm")
category = business.train$categories
removed.tag = c("Restaurants", "Food")
cleaned.category = removeWords(business.train$categories, removed.tag)
cleaned.business$category = as.matrix(as.factor(gsub( "[^[:alnum:]]", "", cleaned.category)))

# Add column checkin to each business
library(stringr)
timestring = str_split(checkin$time,",")
countstring = sapply(sapply(timestring, function(x){str_split(x,":")}), function(x){sapply(x, "[[", 2)}) #2877 list
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

# Clean columns for each attributes
business.allattr = read.csv("~/Desktop/2017_Spring/STAT154/yelp/data/yelp_business_added_attributed_all.csv"
                             ,na.strings=c("", "NA")) #20-71 are attributes
allattr = business.allattr[,-c(1:19)] #2950 business x 52 attr
na.attr = apply(allattr, 2, function(x){sum(is.na(x))})
which(na.attr>nrow(allattr)/3) #which attributes that has more than 1/3 NA values
subattr = allattr[,-which(na.attr>nrow(allattr)/3)] #delet those attribut

business.train.subattr = cbind(cleaned.business, subattr[c(1:nrow(cleaned.business)),]) #2510x30 mat









