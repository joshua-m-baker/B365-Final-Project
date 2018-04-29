#install.packages('mice')
#library('mice')
#setwd('C:\\Users\\Joshua\\Documents\\GitHub\\B365-Final-Project')
library("caret")
options(scipen = 50)
# install.packages("Amelia")
# library("Amelia")

data.train <- read.csv("train_p.csv", header = TRUE)
data.test <- read.csv("test_p.csv", header = TRUE)

test_ids = data.test$id
data.test <- subset(data.test,select = -c(id))
data.train <- subset(data.train,select = -c(id))


training_classes = data.train$target
data.train <- subset(data.train,select = -c(target))
training_size = nrow(data.train)

total_data = rbind(data.train, data.test)
summary(total_data)
#summary(total_data)
# PCA Stuff
pca <- prcomp(total_data, scale.=TRUE)
summary(pca)

#new_data = pca$x[,1:16]
new_data=pca$x

training = as.data.frame(new_data[1:training_size,])
test = new_data[(training_size+1):nrow(new_data),]
d2 = cbind(training, training_classes)
library('e1071')
model = naiveBayes(training_classes~.,data=training)
pred = predict(model, test,"raw")
pred
pred[,2]

m = train(training,as.factor(training_classes),'nb',trControl=trainControl(method='cv',number=10))
p1 = predict(m$finalModel,test)
pred2 = p1$posterior
pred2[,2]

#install.packages("xgboost")
library('xgboost')
head(training)
bstDense <- xgboost(data = as.matrix(training), label = training_classes, missing = NA, max_depth = 5, eta = 1, nthread = 2, nrounds = 10, objective = "binary:logistic")
prediction = predict(bstDense, test)
prediction


submission1 = cbind(id = test_ids, target = pred[,2])
submission2 = cbind(id = test_ids, target = prediction)
write.table(submission1, file="submission1.csv", row.names = FALSE, col.names = TRUE, sep=",")
write.table(submission2, file="submission2.csv", row.names = FALSE, col.names = TRUE, sep=",")


