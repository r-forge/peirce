NlogQ <-
function(N,k){

#------------------------------------------------------------------------------
# this will generate NlogQ from Goulds paper, if desired
# for example k=1; N=10 produces NLQ = 8.58818...

	
cont <- TRUE	# to continue
if (N^N > .Machine$double.xmax){
	res <- paste("Vector of length ",N," is too large. Cannot evaluate.", sep="")
	return(NA) # shorthand, use to test limits of function
	#return(res)
	cont <- FALSE
}
if (!cont) {break}

a <- k^k
b <- N-k
c <- N^N
e1 <-( a*(b^b) )/c

nthroot <- function(a,b) ifelse( b %% 2 == 1 | a >= 0, sign(a)*abs(a)^(1/b), NaN)
Q <- nthroot(e1,N)

a <- 1
b <- 10^a
NLQ <- b+N*log10(Q)

while (NLQ <0){ 
		a <- a+1	
		b <- 10^a
		NLQ <- NLQ <- b+N*log10(Q)
	}

b <- 1
a <- 10^b
NLQ <- a+N*log10(Q)

while (NLQ <0.05){ 
		b <- b+1	
		a <- 10^b
		NLQ <- NLQ <- a+N*log10(Q)
	}

#NLQ2 <- N*log10(Q) # return 'true' value of NlogQ
return(NLQ)
}
