library(digest)
library(bit)

BloomFilter=setRefClass(
  "BloomFilter",
  fields=list(
    .fp_prob="numeric",
    .size="integer",
    .hash_size="integer",
    .bit_array="ANY"
  ),
  methods = list(
    initialize=function(num_items,fp_prob)
    {
      .fp_prob<<-fp_prob
      .size<<-get_size(num_items,fp_prob)
      .hash_size<<-get_hash_size(.size,num_items)
      .bit_array<<-bit(.size)
    },
    get_size=function(n,p)
    {
      m=-(n*log(p))/(log(2)^2)
      return(as.integer(m))
    },
    get_hash_size=function(m,n)
    {
      k=(m/n)*log(2)
      return(as.integer(k))
    },
    get_hash=function(item,seed)
    {
      hash_digest=digest(object=item,serialize = F,seed=seed,algo = "murmur32")
      fin=paste("0x",hash_digest,sep="")
      return(as.numeric(fin)%%.size)
    },
    add=function(item)
    {
      for(i in 1:.hash_size)
      {
        .bit_array[get_hash(item,i)]<<-TRUE
      }
    },
    check=function(item)
    {
      for(i in 1:.hash_size)
      {
        if(.bit_array[get_hash(item,i)]==FALSE)
          return(FALSE)
      }
      return(TRUE)
    }
  )
)
x<-readline(prompt="Enter a name to add: ")
bloom=BloomFilter$new(20,0.05)
bloom$.size
bloom$.hash_size
bloom$add(x)
bloom$check('barry')
bloom$check('harry')
