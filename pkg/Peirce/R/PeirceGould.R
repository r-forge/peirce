PeirceGould <-
function(v, m=1) {

cont <- TRUE	# to continue
if (length(v)^length(v) > .Machine$double.xmax){
	res <- paste("Vector of length ",length(v)," is too large. Cannot evaluate.", sep="")
	return(res)
	cont <- FALSE
}
if (!cont) {break}

#require(NORMT3) # for erfc function

# define functions used further down
nthroot <- function(a,b) ifelse( b %% 2 == 1 | a >= 0, sign(a)*abs(a)^(1/b), NaN)

R1 <- function (x){ #formula for psi.x from Goulds paper
 a <- NORMT3::erfc(x/sqrt(2))
 b <- exp( ( (x^2)-1)/2 )
 R1 <- a*b 
 return(as.real(R1))
 }

R1t <- function (x) {  # wrapper for R1 which returns NA instead of error message
  return(tryCatch(R1(x), error=function(err) NA))
}

sqrt1 <- function (x) { # wrapper for sqrt which returns NA instead of warning
  x1 <- vector(length=length(x))
  for (i in 1:length(x)){
  x1[i] <- tryCatch( sqrt(x[i]), warning=function(w) NA )
  }
  return(x1)
} 


lenv <- length(v)
mo1 <- lenv-2 # maximum no of outliers; need to retain at least 2 points in set
sdv <- sd(v)
meanv <- mean(v)
abserr <- abs(v-meanv) #absolute error
vmat <- cbind(v,abserr)
vmat <- vmat[sort.list(vmat[,2]), ] # sort by abserr

N <- lenv 
k <- seq(from=1, to=mo1,1) # possible no.s to be rejected

#------------------------------------------------------------------

R <- rep(0.99, mo1) # arbitrary starting value
N <- rep(lenv, mo1)

a <- k^k
b <- N-k
c <- N^N
e1 <-( a*(b^b) )/c

nthroot <- function(a,b) ifelse( b %% 2 == 1 | a >= 0, sign(a)*abs(a)^(1/b), NaN)
Q <- nthroot(e1,N)

a <- (Q^N) / (R^k)
b <- N-k
lam <- nthroot(a,b) # lambda from Goulds unlabelled equation above (C.)

m=1
a <- (N-m-k)/k
b <- 1 - lam^2
xsq <- 1 + a*b

#if (xsq<0 | xsq==1){
#	print(errmess)
#	}

x <- sqrt1(xsq)

refine <- function( x, R, Q, N, k ){ # following Goulds method 
	
 	repeat{
		R <- R1t(x)

		a <- (Q^N) / (R^k)
		b <- N-k;b
		lam <- nthroot(a,b)

		a <- (N-m-k)/k
		b <- 1 - lam^2
		xsq <- 1 + a*b
		x <- sqrt1(xsq)
    
	if ( is.na(x) ){break}
	if ( is.na(R1t(x)) ) {break}
	if ( abs(R1t(x)-R) <1e-5 ) {break}
 }
return(x)} # returns R, the value given in Ross' table

m1 <- mapply(FUN=refine, x, R, Q, N, k)
m1sd <- sdv*m1
m2 <- append(m1sd, rep(NA,2))
vmat2 <- cbind(vmat,rev(m2))

r1 <- is.na(vmat2[,3]) | vmat2[,2] < vmat2[,3]
res <- vmat2[,1][r1]
return(res)
}
