setwd("F://R//files")
glass <- read.csv("glass.csv")
View(glass)

table(glass$Type)
str(glass)

glass$Type <- factor(glass$Type)

#perform normalization

norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

glass_n <- as.data.frame(lapply(glass[1:9],norm))

View(glass_n)
summary(glass_n)

glass <- data.frame(glass_n, glass[,10])
View(glass)
#seperate test and train data
library(caret)
sample <- createDataPartition(glass$glass...10., p= 0.70, list = FALSE)
glass_train <- glass[sample,]
glass_test <- glass[-sample,]

View(glass_test)
glass_train_data <- glass_train[,-10]
glass_test_data <- glass_test[,-10]


glass_train_labels <- glass_train[,10]
glass_test_labels <- glass_test[,10]

#modelbuilding
library(class)


model <- knn(train = glass_train_data, test = glass_test_data , cl = glass_train_labels, k = 5)
  
summary(model)

#evaluation
library(gmodels)

CrossTable(x = glass_test_labels, y = model)
a <- table(glass_test_labels, model)

acc <- sum(diag(a))/sum(a)
acc #60.65

model2 <- knn(train = glass_train_data, test = glass_test_data , cl = glass_train_labels, k = 7)

summary(model2)

#evaluation
library(gmodels)

CrossTable(x = glass_test_labels, y = model2)
a1 <- table(glass_test_labels, model)

acc1 <- sum(diag(a1))/sum(a1)
acc1 #68
