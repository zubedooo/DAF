library(MASS)
library(ggplot2)
data = read.csv("6.csv")

dim(data)
str(data)
summary(data)

attach(data)
data

set.seed(1)
row.number = sample(1:nrow(data),0.6*nrow(data))
row.number
train = data[row.number,]
test = data[-row.number,]

dim(train)
dim(test)

attach(train)
model1 = glm(default~student+balance+income,data = train,family = binomial)
summary(model1)
model1


pred.prob = predict(model1,type = "response")
pred.prob = ifelse(pred.prob>0.5,1,0)
table(pred.prob,train$default)

attach(test)
pred.prob = predict(model1,newdata = test,type = "response")
pred.prob = ifelse(pred.prob>0.5,1,0)
table(pred.prob,test$default)


