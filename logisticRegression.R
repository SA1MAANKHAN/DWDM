library(ggplot2)
library(cowplot)

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

# Generalized Linear Model 
formula <- status~.
logit <- glm(formula, data = data_train, family = 'binomial')
summary(logit)

predict <- predict(logit, data_test, type = 'response')
# confusion matrix

table_mat <- table(data_test$status, predict > 0.5)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

precision <- function(matrix) {
  # True positive
    tp <- matrix[2, 2]
    # false positive
    fp <- matrix[1, 2]
    return (tp / (tp + fp))
    }
recall <- function(matrix) {
   # true positive
      tp <- matrix[2, 2]# false positive
         fn <- matrix[2, 1]
         return (tp / (tp + fn))
}

prec <- precision(table_mat)
prec

rec <- recall(table_mat)
rec
  