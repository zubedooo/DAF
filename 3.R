bank<-read.csv("3.csv",header=TRUE,sep=",")
summary(bank)

dis <- transform(bank, distance= sqrt((x-bank$age)^2+(y-bank$loan)^2 ) )
dis
odis<- dis[order(dis$distance),c(3,4)]
odis

k = 3
nn<-head(odis,k)
nn
knn<-table(nn$defaulter)
knn
t<-names(which(knn==max(knn)))
tss
cat("class:",t[[1]][1])
