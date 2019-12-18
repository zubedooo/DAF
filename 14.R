library(digest)

n=10
m=100
nh=4
l = rep(0,m)
l

URL_clicked=c('www.google.com','www.facebook.com','www.yahoo.com','www.gamespot.com','www.ign.com',
                'www.stayfit.com','www.gamer.com','www.gmail.com','www.outlook.com','www.mailer.com')

print(URL_clicked)

URL_notclicked=c('www.insider.com','www.outsider.com','www.paytm.com','www.gpay.com','www.gameology.com',
               'www.youpay.com','www.msrit.com','www.theforum.com','www.bms.com','www.pvr.com')

print(URL_notclicked)

get_hash = function(item,seed){
  hex_str=digest(object=item,
                 algo="murmur32",
                 serialize=F,
                 seed=seed)
  hex=paste('0x',hex_str,sep="")
  return(as.numeric(hex) %% m)
  
  
}

add = function(item){
  for (i in 1:nh){
    hash_digest=get_hash(item,i)
    hash_digest=hash_digest+1
    l[hash_digest]<<-1
  }
}

check = function(item){
  for(i in 1:nh){
    hash_digest=get_hash(item,i)
    hash_digest=hash_digest+1
    if(l[hash_digest] == 0)
    {
      return(FALSE)
    }
  }
  return(TRUE)
  
}

for(i in 1:n){
  add(URL_clicked[i])
}

print(l)
test_set=c(URL_clicked[1:5],URL_notclicked)
print(test_set)

for(i in 1:length(test_set)){
  if(check(test_set[i]))
  {
    if(test_set[i] %in% URL_notclicked)
    {
      cat(test_set[i]," this is clicked ","\n")
    }
    else
    {
      cat(test_set[i]," this is probably clicked ","\n")
    }
  }
  else
  {
    cat(test_set[i]," this is not clicked !","\n")
  }
}
l
