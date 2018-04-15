data.train <- read.csv("train.csv", header = TRUE,na.strings = "-1")
data.test <- read.csv("test.csv", header = TRUE,na.strings = "-1")


data.test = cbind(data.test,NA_count = rowSums(is.na(data.test))) 
data.train = cbind(data.train,NA_count = rowSums(is.na(data.train))) 


b = c(ps_car_03_cat,ps_car_05_cat)
data.test <- subset(data.test,select = -c(ps_car_03_cat,ps_car_05_cat))
data.train <- subset(data.train,select = -c(ps_car_03_cat,ps_car_05_cat))


