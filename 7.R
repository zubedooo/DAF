library(MASS)
library(ggplot2)
data = read.csv("7.csv")

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

model2 = lda(default~student+balance+income,data=train)
model2
summary(model2)

attach(train)
pred1 = predict(model2,data = train)
table(pred1$class,default)

attach(test)
pred2 = predict(model2,newdata = test)
pred2
table(pred2$class,default)

model2
par(mfrow=c(1,1))
plot(model2$x[,1], model2$class, col=test$default)
