#install.packages('mice')
#library('mice')
#setwd('C:\\Users\\Joshua\\Documents\\GitHub\\B365-Final-Project')

data.train <- read.csv("train.csv", header = TRUE,na.strings = "-1" )
data.test <- read.csv("test.csv", header = TRUE,na.strings = "-1")
data.train[data.train<0] = NA
data.test[data.test<0] = NA

data.test = cbind(data.test,NA_count = rowSums(is.na(data.test))) 
data.train = cbind(data.train,NA_count = rowSums(is.na(data.train))) 


data.test <- subset(data.test,select = -c(id,ps_car_03_cat,ps_car_05_cat,ps_reg_03))
data.train <- subset(data.train,select = -c(id,ps_car_03_cat,ps_car_05_cat,ps_reg_03))

training_classes = data.train$target
data.train <- subset(data.train,select = -c(target))
training_size = length(data.train)

total_data = rbind(data.train, data.test)

col_means = colMeans(total_data,na.rm = TRUE)
for(i in 1:ncol(total_data)){
  total_data[is.na(total_data[,i]), i] <- col_means[i]
}

summary(total_data)
# PCA Stuff
pca <- prcomp(total_data)
summary(pca)
