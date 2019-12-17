setwd("C:\\Users\\gaura\\OneDrive\\Documents\\DA_Final_Lab")
lr<-read.csv("2.csv",header=TRUE,sep=",")
lr

x1 <- lr$subject1
x2 <- lr$subject2
y <- lr$subject3

X1 <- mean(x1)
X2 <- mean(x2)
Y <- mean(y)

num1 = sum((x1-X1)*(y-Y))
dem1 = sum((x1-X1)^2)
b1 = num1/dem1
b1

num2 = sum((x2-X2)*(y-Y))
dem2 = sum((x2-X2)^2)
b2 = num2/dem2
b2

b0 = Y - b1*X1 - b2*X2
b0

lr$pred = b0 + b1*x1 + b2*x2
lr


rss = sum((y-lr$pred)^2)
rss
tss = sum((y-Y)^2)
tss
rse = sqrt(rss/(nrow(lr)-2))
rse
se = 1 -  (rss/tss)
se

mod1 <-lm(y~x1+x2,data=lr)
summary(mod1)
lr$pred2 <- predict(mod1,data.frame(x1=c(x1),x2=c(x2)))
                                     
lr
summary(lr)
plot(lr$pred,y)
plot(lr$pred,y,xlab="predicted",ylab="actual")
abline(a=0,b=1)
