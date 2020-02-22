setwd("F://R//Rfiles")
Zoodata <- read.csv("Zoo.csv")
View(Zoodata)

 
str(Zoodata)
table(Zoodata$type)
Zoodata$type <- factor(Zoodata$type)

str(Zoodata)

#to normalize data
norm <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

Zoodata_n <- as.data.frame(lapply(Zoodata[2:17],norm))
View(Zoodata_n)
Zoodata <- data.frame(Zoodata_n, Zoodata[,18])
View(Zoodata)
#seperate test and train data
library(caret)
sample <- createDataPartition(Zoodata$Zoodata...18., p = 0.75, list = FALSE)
sum(is.na(sample))
Zoo_train <- Zoodata[sample,]
Zoo_test <- Zoodata[-sample,]
View(Zoo_test)
Zoo_train_labels <- factor(Zoo_train[,17])
Zoo_test_labels <- factor(Zoo_test[,17])

table(Zoo_test_labels)
#to build a model
install.packages("class")
install.packages("caret")
library(class)
library(caret)

Zoomodel1 <- knn(train=Zoo_train, test = Zoo_test, cl= Zoo_train_labels, k=5)


#evaluation

install.packages("gmodels")
library(gmodels)
a <- CrossTable(x = Zoo_test_labels, y = Zoomodel1)
b <- table(Zoo_test_labels, Zoomodel1)

acc <- sum(diag(b))/sum(b)
acc
 #95.833