#install.packages('mice')
#library('mice')
#setwd('C:\\Users\\Joshua\\Documents\\GitHub\\B365-Final-Project')
library("caret")
options(scipen = 50)
# install.packages("Amelia")
# library("Amelia")

data.train <- read.csv("train.csv", header = TRUE,na.strings = "-1" )
data.test <- read.csv("test.csv", header = TRUE,na.strings = "-1")
data.train[data.train<0] = NA
data.test[data.test<0] = NA

data.test = cbind(data.test,NA_count = rowSums(is.na(data.test))) 
data.train = cbind(data.train,NA_count = rowSums(is.na(data.train))) 
plot(data.train$NA_count, data.train$target)

test_ids = data.test$id
data.test <- subset(data.test,select = -c(id))
data.train <- subset(data.train,select = -c(id))

training_classes = data.train$target
data.train <- subset(data.train,select = -c(target))
training_size = nrow(data.train)

data.test <- subset(data.test,select = -c(ps_car_03_cat,ps_car_05_cat,ps_reg_03))
data.train <- subset(data.train,select = -c(ps_car_03_cat,ps_car_05_cat,ps_reg_03))

total_data = rbind(data.train, data.test)
# total_data <- amelia(total_data, idvars = c("ps_ind_14", "NA_count", "ps_ind_09_bin"), m=3, p2s = 1)
# summary(total_data)
col_means = colMeans(total_data, na.rm = TRUE)
for(i in 1:ncol(total_data)){
  total_data[is.na(total_data[,i]), i] <- col_means[i]
}

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

install.packages("xgboost")
library('xgboost')
head(training)

bstDense <- xgboost(data = as.matrix(training), label = training_classes, missing = NA, max_depth = 10, eta = 1, nthread = 2, nrounds = 5, objective = "binary:logistic")
prediction = predict(bstDense, test)
prediction


#submission = cbind(id = test_ids, target = pred[,2])
submission = cbind(id = test_ids, target = prediction)
write.table(submission, file="submission.csv", row.names = FALSE, col.names = TRUE, sep=",")


