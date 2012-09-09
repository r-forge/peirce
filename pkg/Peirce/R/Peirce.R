Peirce <-
function(v, m=1) {

## the complementary error function
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower.tail = FALSE)

lenv <- length(v)
mo1 <- lenv-1 # maximum no of outliers; need to retain at least 2 points in set
sdv <- sd(v)
meanv <- mean(v)
abserr <- abs(v-meanv) #absolute error
vmat <- cbind(v,abserr)
vmat <- vmat[sort.list(vmat[,2]), ] # sort by abserr

findx <- function(N,k,m){ # method of  Knud Thomsen

x <- 1
if ((N - m - k) <= 0) {
	return(NaN)
	print(NaN)
}  else {
	x    <- min(x, sqrt((N - m)/k) - 1e-10)
	LnQN <- k * log(k) + (N - k) * log(N - k) - N * log(N)
	R1   <- exp((x^2 - 1)/2) * erfc(x/sqrt(2))
	R2   <- exp( (LnQN - 0.5 * (N - k) * log((N-m-k*x^2)/(N-m-k)) )/k )
	R1d  <- x * R1 - sqrt(2/pi/exp(1))
	R2d  <- x * (N - k)/(N - m - k * x^2) * R2
	oldx <- x
	x    <- oldx - (R1 - R2)/(R1d - R2d)
	while (abs(x - oldx) >= N * 2e-16){ # 2e-16) { # arbitrary liimit of resolution
		R1   <- exp((x^2 - 1)/2) * erfc(x/sqrt(2))
		R2   <- exp( (LnQN - 0.5 * (N - k) * log((N-m-k*x^2)/(N-m-k)) )/k )
		R1d  <- x * R1 - sqrt(2/pi/exp(1))
		R2d  <- x * (N - k)/(N - m - k * x^2) * R2
		oldx <- x
		x    <- oldx - (R1 - R2)/(R1d - R2d)
		}
	}
return(x) # this is R from Ross paper
}

findxT <- function(N1,k1,m1){
r1 <- tryCatch( findx(N=N1,k=k1,m=m1), warning=function(w) NA, error=function(err) NA)
return(r1)
}

# vectorize the function
N1 <- rep(lenv, mo1)
k1 <- seq(from=1, to=mo1, by=1)
m1 <- rep(m, mo1)

m1 <- mapply(FUN=findxT, N1,k1,m1)

m1sd <- sdv*m1
#m2 <- append(m1sd, rep(NA,2)) # add NA, NA as must leave 2x observations
m2 <- append(m1sd, rep(NA)) # add NA, NA as must leave 2x observations
vmat2 <- cbind(vmat,rev(m2))

r1 <- is.na(vmat2[,3]) | vmat2[,2] < vmat2[,3] # logical vector indicating values to return
res <- vmat2[,1][r1] # apply this to original vector
return(res)
}
