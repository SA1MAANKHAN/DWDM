library(naivebayes)
library(dplyr)
library(ggplot2)

summary(data)

head(data)

data_filtered <- subset(data,select=c(2,3,5,7,8,9,10,12,13,14))

data_filtered$status <- as.factor(data_filtered$status)

head(data_filtered)

is.factor(data_filtered$status)

continuous <-select_if(data_filtered, is.numeric)

summary(continuous)

library(ggplot2)

ggplot(continuous, aes(x = hsc_p)) + geom_density(alpha = .2, fill = "#FF6666")
ggplot(continuous, aes(x = degree_p)) + geom_density(alpha = .2, fill = "#FF6666")


# CHECK CATEGORICAL VARIABLE
factor <- data.frame(select_if(data_filtered, is.factor))

ncol(factor)

sapply(data_filtered, class)

data_filtered$hsc_s<- as.factor(data$hsc_s)
data_filtered$degree_t<- as.factor(data$degree_t)
data_filtered$workex<- as.factor(data$workex)
data_filtered$specialisation<- as.factor(data$specialisation)
data_filtered$specialisation<- as.factor(data$specialisation)
data_filtered$status<- as.factor(data$status) 
factor <- data.frame(select_if(data_filtered, is.factor))

ncol(factor)

set.seed(1234)

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <-  1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

data_test <- create_train_test(data_filtered, 0.8, train = FALSE)
data_train <- create_train_test(data_filtered, 0.8, train = TRUE)
dim(data_train)

dim(data_test)

dim(data_filtered)

model <- naiveBayes (status ~ ., data = data_train)

model

plot(model) 



predict(model, data_test)


p1 <- predict(model, data_train)

(tab1 <- table(p1, data_train$status))

1 - sum(diag(tab1)) / sum(tab1)

accuracy_Test <- sum(diag(tab1)) / sum(tab1)
accuracy_Test

