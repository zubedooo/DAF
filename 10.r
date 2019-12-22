x=c(1,2,3,4,5,6,7,8,9,10)

hash=function(var)
{
  return((6*var+1)%%23)
}

tobit=function(var)
{
  val=intToBits(var)
  val=paste(as.integer(val),collapse="")
  return(val)
}

max=0
count_max=function(var)
{
  count=0
  var=strsplit(var,split="")
  for(i in 1:length(var[[1]]))
  {
    if(var[[1]][i]!=1)
    {
      count=count+1
    }
  }
  return(32-count)
}


h=c()
for(i in 1:length(x))
{
  temp=hash(x[i])
  h[i]=temp
}
bin=c()
for(i in 1:length(x))
{
  temp=tobit(h[i])
  bin[i]=temp
}
for(i in 1:length(bin))
{
  c=count_max(bin[i])
  if(c>max)
    max=c
}
2^max
