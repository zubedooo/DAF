df=read.csv("5.csv",header=TRUE,sep=",")
df

#using inbuilt functions

mlr.model=lm(Sales~TV+Radio,data=df)
test=data.frame(TV=df$TV,Radio=df$Radio)
mlr.pred=predict(mlr.model,test)
mlr.model

#without inbuilt functions

tv_mean=mean(df$TV)
radio_mean=mean(df$Radio)
sales_mean=mean(df$Sales)
m1=sum((df$TV-tv_mean)*(df$Sales-sales_mean))/sum((df$TV-tv_mean)**2)
m1
m2=sum((df$Radio-radio_mean)*(df$Sales-sales_mean))/sum((df$Radio-radio_mean)**2)
m2
m0=sales_mean-m1*tv_mean-m2*radio_mean
m0
pred=m0+df$TV*m1+df$Radio*m2
