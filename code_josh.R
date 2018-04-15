library(corrplot)
library(data.table)
setwd('C:\\Users\\Joshua\\Documents\\GitHub\\B365-Final-Project')
data.train = fread('train.csv')
data.test = fread('test.csv')
summary(data.train)
head(data.train)
summary(data.test)

data.train[data.train == -1] <- NA
data.train[data.train == -1.0] <- NA

data.reduced = na.omit(data.train)
x = cor(data.reduced)
corrplot(x, method = "circle", type = 'upper')

library('caret')
tr = data.train
tr$target = as.factor(tr$target)
print(Sys.time())
#load('model1.rda')
model = train(tr[,3:59],tr$target,'nb',preProcess = 'knnImpute',
              trControl = trainControl(method="cv",number=10))
save(model, file = 'model1.rda')
print(Sys.time())
              