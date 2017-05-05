
### Clean user matrix and clustering
user.use = user[,-c(1,11,22)]

yelp_since=as.integer(gsub("-","",x=as.vector(as.character(user[,which(colnames(user)=="yelping_since")]))))
elitesum = sapply(user$elite, function(x){length(str_split(x,",")[[1]])})
friendsum = sapply(user$friends, function(x){length(str_split(x,",")[[1]])})
user.use$elite = elitesum  #replace elite to number of years user is elite
user.use$yelping_since = yelp_since #replace yelping_since
user.use$friends = friendsum #replace friends 
colnames(user.use)

# K-means 
numCenters = 3
kmeans = kmeans(user.use[,-15], numCenters, iter.max = 20, nstart = 1)

kmeans$size
kmeans$centers

# Create new matrix
user.new = cbind(user.use[,c(15,1:14,16:21)], as.integer(kmeans$cluster))
colnames(user.new)[22]=c("kmcluster")


# Match user to review matrix (text.m)
text.m$user_id = review.train$user_id
text.m$usercluster = rep(0, nrow(text.m))
for (i in 1:nrow(user.new)){
  if (user.new$user_id[i] %in% text.m$user_id){
    text.m$usercluster[which(text.m$user_id == toString(user.new$user_id[i]))] = user.new$kmcluster[i]
  }
}

