df=read.csv("7.csv",header=TRUE,sep=",")
df
#Not required but improves results, kinda
#mins=apply(df[,4:5],2,min)
#maxs=apply(df[,4:5],2,max)

#dataset=as.data.frame(scale(df[,4:5],center = mins,scale = (maxs-mins)))
#dataset$default=as.factor(df$default)
#dataset$student=as.factor(df$student)

#str(dataset)
#df=dataset
index=sample(1:nrow(df),round(nrow(df)*0.6))
df_train=df[index,]
df_test=df[-index,]
df_train
library(MASS)
model=lda(default~student+balance+income,data=df_train)
model
pred=predict(model,newdata = df_test)
pred$class
table(actual=df_test$default,pred=pred$class)
