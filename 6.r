df=read.csv("6.csv",header=TRUE,sep=",")
df
df$student=ifelse(df$student=='Yes',1,0)
df$student=as.factor(df$student)
df$default=ifelse(df$default=='Yes',1,0)
df$default=as.factor(df$default)
df

index=sample(1:nrow(df),round(nrow(df)*0.6))
df_train=df[index,]
df_test=df[-index,]
#logistic regression
model=glm(default~student+balance+income,data = df_train,family="binomial")

pred=predict(model,type = 'response',newdata = df_test)
pred
y_pred=ifelse(pred>0.5,1,0)
table(y_pred,df_test$default)
