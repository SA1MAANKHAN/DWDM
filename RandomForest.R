head(data)

library(rpart)
library(rpart.plot)

data_filtered <- subset(data,select=c(2,3,5,7,8,9,10,12,13,14))

is.factor(data_filtered$status)

head(data_filtered)

# created a function for train test split 
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_train <- create_train_test(data_filtered, 0.8, train = TRUE)
data_test <- create_train_test(data_filtered, 0.8, train = FALSE)

library(randomForest)
library(e1071)



