df=read.csv("2.csv",header=TRUE,sep=",")

#using prebuilt function
df
test=data.frame(experience=df$experience,training=df$training)
mlr.model=lm(publications~experience+training,data=df)
mlr.model

#Without prebuilt functions
pub_mean=mean(df$publications)
exp_mean=mean(df$experience)
train_mean=mean(df$training)

m1=sum((df$experience-exp_mean)*(df$publications-pub_mean))/sum((df$experience-exp_mean)**2)
m1
m2=sum((df$training-train_mean)*(df$publications-pub_mean))/sum((df$training-train_mean)**2)
m2
m0=pub_mean-m1*exp_mean-m2*train_mean
m0
pred=m0+df$experience*m1+df$training*m2
pred

pred2=predict(mlr.model,test)
pred2
