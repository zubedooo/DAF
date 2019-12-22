lr<-read.csv("12.csv",header=TRUE,sep=",")
lr

x<-lr$subject1
y<-lr$subject2


X <- mean(x)
X
Y <- mean(y)
Y

num = sum((x-X)*(y-Y))
dem = sum((x-X)^2)

b1 = num/dem
b0 = Y - b1*X
b1
b0
lr$pred = b0 + b1*x
lr
rss = sum((y-lr$pred)^2)
rss
tss = sum((y-Y)^2)
tss
rse = sqrt(rss/(nrow(lr)-2))
rse
se = 1 -  (rss/tss)
se


fit <- lm(y~x,lr)
summary(fit)

lr$pred2 = predict(fit,data.frame(x=c(x)))
lr


plot(x,y)
lines(x,lr$pred)
lines(x,lr$pred2)
