setwd("~/Desktop/Stats 154 Final Project/Dataset")
review.pred.boosting_7 <- read.csv("gbr_d7_test_pred.csv", header = FALSE)

review.pred <- NULL
all.pred <- NULL
all.pred.mean <- NULL
all.pred.mean <- data.frame(rating = review.pred.boosting_7$V1)
review.pred <- cbind(all.pred.mean, business_id = review.test$business_id)
output <- vector("list", length = 440)

# Sort all reviews by their business ID
for (i in 1:length(business.test$business_id)) {
  for (j in 1:length(review.pred$business_id)) {
    if (business.test$business_id[i] == as.character(review.pred$business_id[j])) {
      len <- length(output[[i]])
      output[[i]][len + 1] <- review.pred$rating[j]
    }
  }
}

output <- lapply(output, mean)

# Round to the nearest 0.5 start
for (i in 1:length(output)) {
  if (output[[i]] < 0.25) {
    output[[i]] = 0
  } else if (output[[i]] >= 0.25 & output[[i]] < 0.75) {
    output[[i]] = 0.5
  } else if(output[[i]] >= 0.75 & output[[i]] < 1.25) {
    output[[i]] = 1
  } else if (output[[i]] >= 1.25 & output[[i]] < 1.75) {
    output[[i]] = 1.5
  } else if (output[[i]] >= 1.75 & output[[i]] < 2.25) {
    output[[i]] = 2
  } else if (output[[i]] >= 2.25 & output[[i]] < 2.75) {
    output[[i]] = 2.5
  } else if (output[[i]] >= 2.75 & output[[i]] < 3.25) {
    output[[i]] = 3
  } else if (output[[i]] >= 3.25 & output[[i]] < 3.75) {
    output[[i]] = 3.5
  } else if (output[[i]] >= 3.75 & output[[i]] < 4.25) {
    output[[i]] = 4
  } else if (output[[i]] >= 4.25 & output[[i]] < 4.75) {
    output[[i]] = 4.5
  } else if (output[[i]] >= 4.75) {
    output[[i]] = 5
  }
}

final.pred = data.frame(business_id = business.test$business_id, stars = unlist(output))

# compare with previous model
last.pred = read.csv("prediction_boosting_d15.csv")
mean((last.pred$stars - final.pred$stars)^2)

write.csv(final.pred, file = "prediction_boosting_d7.csv")