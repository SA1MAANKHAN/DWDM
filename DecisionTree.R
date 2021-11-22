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

#use the function prop.table() combined with table() to verify if the randomization process is correct.
prop.table(table(data_train$status))

#fitting model
fit <- rpart(status~., data = data_train, method = 'class')

rpart.plot(fit, extra = 106)

predict_unseen <-predict(fit, data_test, type = 'class')

table_mat <- table(data_test$status, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

print(paste('Accuracy for test', accuracy_Test))


