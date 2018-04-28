#install.packages('mice')
#library('mice')
#setwd('C:\\Users\\Joshua\\Documents\\GitHub\\B365-Final-Project')
options(scipen = 50)

data.train <- read.csv("train.csv", header = TRUE,na.strings = "-1" )
data.test <- read.csv("test.csv", header = TRUE,na.strings = "-1")
data.train[data.train<0] = NA
data.test[data.test<0] = NA

data.test = cbind(data.test,NA_count = rowSums(is.na(data.test))) 
data.train = cbind(data.train,NA_count = rowSums(is.na(data.train))) 

test_ids = data.test$id

data.test <- subset(data.test,select = -c(id,ps_car_03_cat,ps_car_05_cat,ps_reg_03))
data.train <- subset(data.train,select = -c(id,ps_car_03_cat,ps_car_05_cat,ps_reg_03))

training_classes = data.train$target
data.train <- subset(data.train,select = -c(target))
training_size = nrow(data.train)

total_data = rbind(data.train, data.test)

col_means = colMeans(total_data,na.rm = TRUE)
for(i in 1:ncol(total_data)){
  total_data[is.na(total_data[,i]), i] <- col_means[i]
}

#summary(total_data)
# PCA Stuff
pca <- prcomp(total_data)
#summary(pca)

new_data = pca$x[,1:5]

training = as.data.frame(new_data[1:training_size,])
test = new_data[(training_size+1):nrow(new_data),]
d2 = cbind(training, training_classes)
library('e1071')
model = naiveBayes(training_classes~.,data=training)
pred = predict(model, test,"raw")
pred
pred[,2]

submission = cbind(id = test_ids, target = pred[,2])
write.table(submission, file="submission.csv", row.names = FALSE, col.names = TRUE, sep=",")
#model = train(training,training_classes,'nb',trControl=trainControl(method='cv',number=10))

