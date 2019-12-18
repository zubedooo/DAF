library(MASS)
library(ggplot2)
data = read.csv("8.csv")

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

model3 = qda(default~student+balance+income,data=train)
model3
summary(model3)

attach(train)
pred3 = predict(model3,data = train)
table(pred3$class,default)

attach(test)
pred4 = predict(model3,newdata = test)
pred4
table(pred4$class,default)


par(mfrow=c(1,1))
plot(pred4$posterior[,2], pred4$class, col=test$default)
