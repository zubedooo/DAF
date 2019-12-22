df=read.csv("12.csv",header=TRUE,sep=",")

#Using predefined functions

lr.model=lm(subject1~subject2,data = df)
test=data.frame(subject2=df$subject2)
lr.pred=predict(lr.model,test)
plot(test$subject1,df$subject2,col="blue")
lines(test$subject1,lr.pred,col="red")

#Without Prebuilt functions

exp_mean=mean(df$subject1)
pub_mean=mean(df$subject2)
m1=sum((df$subject1-exp_mean)*(df$subject2-pub_mean))/sum((df$subject1-exp_mean)^2)
m0=pub_mean-exp_mean*m1
pred=m0+m1*df$subject1
plot(df$subject1,df$subject2)
lines(df$subject1,pred)

rss=sum((df$subject1-pred)^2)
tss=sum((df$subject2-pub_mean)^2)
se=1-(rss/tss)
rse=(rss/(nrow(df)-2))
