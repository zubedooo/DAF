x<-c("Hello","world","Ralph","Internet")
x<-as.numeric(as.factor(x))
print(x)
h<-function(var){
  return ((6*var+1)%%5)
}
H<-c()
for(i in 1:length(x)){
  H[i]<-h(x[i])
}
b<-function(var){
  l1=as.numeric(intToBits(var))
  index<-match(1,l1)-1
  return (index)
}
B<-c()
for(i in 1:length(H)){
  B[i]<-b(H[i])
}
B[is.na(B)]<-0
print(2^max(B))
