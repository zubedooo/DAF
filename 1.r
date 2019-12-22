df=read.csv("1.csv",header=TRUE,sep=",")

#Using predefined functions

lr.model=lm(publications~experience,data = df)
test=data.frame(experience=df$experience)
lr.pred=predict(lr.model,test)
plot(test$experience,df$publications,col="blue")
lines(test$experience,lr.pred,col="red")

#Without Prebuilt functions

exp_mean=mean(df$experience)
pub_mean=mean(df$publications)
m1=sum((df$experience-exp_mean)*(df$publications-pub_mean))/sum((df$experience-exp_mean)^2)
m0=pub_mean-exp_mean*m1
pred=m0+m1*df$experience
plot(df$experience,df$publications)
lines(df$experience,pred)

rss=sum((df$publications-pred)^2)
tss=sum((df$publications-pub_mean)^2)
se=1-(rss/tss)
rse=(rss/(nrow(df)-2))
