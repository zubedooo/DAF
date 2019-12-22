df=read.csv("4.csv",header=TRUE,sep=",")

#using inbuilt functions

lr.model=lm(sales~budget,data=df)
test=data.frame(budget=df$budget)
lr.pred=predict(lr.model,test)
plot(df$budget,df$sales)
lines(df$budget,lr.pred)
lr.model
#without using prebuilt functions

budget_mean=mean(df$budget)
sale_mean=mean(df$sales)
m1=sum((df$budget-budget_mean)*(df$sales-sale_mean))/sum((df$budget-budget_mean)^2)
m1
m0=sale_mean-budget_mean*m1
m0
pred=m0+df$budget*m1
plot(df$budget,df$sales)
lines(df$budget,pred)
