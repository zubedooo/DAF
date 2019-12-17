set.seed(123456789)

n <- 16

probabilisticCounting = function(stream) {
	bits <- rep(0, n)
	for (s in stream) {
		bits[p(binary(hash(s)))] <- 1
	}
	print(bits)
	return (1/0.7735 * 2^p(bits))
}

hash <- function(x) {
	 # get around 32-bit limitation - using substr
	strtoi(substr(digest(x, algo="crc32"), 0, 7), 16L)
}

# position of first bit equal 0
p <- function(b) {
	zeroIndex <- which(b != 1)
	firstZero <- min(zeroIndex)
	return(firstZero)
}

binary <- function(x) {
	b <- rev(as.numeric(intToBits(x)))
	# get around 32-bit limitation
	b <- tail(b, 16) 
	return(b)
}

stream = as.integer(runif(10000, 0, 2^n))
probabilisticCounting(stream)
