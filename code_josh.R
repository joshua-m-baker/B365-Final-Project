library(corrplot)
data.train = read.csv('C:\\Users\\Joshua\\Documents\\Data Mining Final Project\\train.csv')
summary(data.train)
head(data.train)

data.train[data.train == -1] <- NA
data.train[data.train == -1.0] <- NA

data.reduced = na.omit(data.train)
x = cor(data.reduced)
corrplot(x, method = "circle", type = 'upper')
