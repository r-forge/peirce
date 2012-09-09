PeirceLimits <-
function(N=1000L, plots=TRUE, noPoints=100,asPercent=TRUE){

    findx <- function(k,m,N){ # method of  Knud Thomsen

        erfc <- function(x) 2 * pnorm(x * sqrt(2), lower.tail = FALSE) # the complementary error function

        x <- 1                          #arbitrary starting value
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
            while (abs(x - oldx) >= N * 2e-16) { # arbitrary liimit of resolution
		R1   <- exp((x^2 - 1)/2) * erfc(x/sqrt(2))
		R2   <- exp( (LnQN - 0.5 * (N - k) * log((N-m-k*x^2)/(N-m-k)) )/k )
		R1d  <- x * R1 - sqrt(2/pi/exp(1))
		R2d  <- x * (N - k)/(N - m - k * x^2) * R2
		oldx <- x
		x    <- oldx - (R1 - R2)/(R1d - R2d)
            }
	}
        return(x)                       # this is R from Ross paper
    }

    k <- seq.int(from=1L, to=N, length.out=noPoints)
    m <- seq.int(from=1L, to=N, length.out=noPoints)

    res <- outer(k,m,FUN=findx,N=N)

    if (isTRUE(asPercent)){
        kper <- 100*k/N                 # express as percent
        mper <- 100*m/N
        rownames(res) <- kper
        colnames(res) <- mper
    }
    res <- reshape::melt(res)
    colnames(res) <- c("k","m","R")
    df1 <- as.data.frame(res)
    df1 <- df1[complete.cases(df1),]
    len1 <- length(df1$k)

if(isTRUE(plots)){
    rgl::bg3d("white")
    rgl::plot3d(x=df1$k, z=df1$m, y=df1$R, col=rainbow(len1), xlab="", zlab="", ylab="",main="",sub=" ")
    rgl::par3d(cex=2)
    rgl::decorate3d(xlab="           k", zlab="                  m", ylab="")
    rgl::bbox3d(color=c("#333377","black"), emission="#333377",specular="#3333FF", shininess=5, alpha=0.8) # background box for highlighting
    rgl::text3d(x=80,y=2,z=80, texts="R")

### snapshot3d(filename="Rmk.png") # use this to save copy of image

### alternative
    scatterplot3d::scatterplot3d(x=df1$k, y=df1$m, z=df1$R, highlight.3d=TRUE, grid=TRUE)
}

    return(df1)
}
