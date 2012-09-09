Peirce1852 <-
function(width=1366, height=768) {

# require Hsmisc # for labcurve
 require(ggplot2)
 require(NORMT3) # for erfc
 require(pracma) # for quadinf

#--------------------------------
# set up graphical parameters
#--------------------------------

windows(record=T, width=width, height=height) #ratio is 1.778

#-----------------------------------------------------------
# phi function from paper 
#-----------------------------------------------------------

phix <- function(err,me){ # err=error, me=mean error
	a <- 1/(me * sqrt(2*pi))
	b <- exp (-err^2 / (2*me^2))
	phi <- a*b
	return(phi)
	}

#--------------------------------------------------
# initial plot with labcurve

err1 <- seq(from=0,to=4, by = 0.5)
me1 <- seq(1, 5, by=1)
l1 <- list(); l2 <- list()
for (i in me1){
  p1 <- phix(err=err1,me=i)
  l1$y <- p1 
  l1$x <- err1
  l2[[i]] <- list(x=l1$x, y=l1$y) 
  }

names(l2) <- format(me1)
xn1 <- expression(paste("Error  ", delta))
yn1 <- expression(paste("Probability  ", phi))
tit1 <- expression(paste("Probability ",phi," of error ",delta, " occurring in system with mean error ", epsilon) )

p <- par
par(oma=c(1,1,2,1))

Hmisc::labcurve(curves=l2, pl=TRUE, xlab=xn1, ylab=yn1, method="on top",keys=me1, col=me1+1, cex=1.3 )
mtext(tit1, outer=TRUE, cex=2)

err1 <- seq(from=1,to=20, by = 0.2)
me1 <- seq(1, 10, by=1)
l1 <- list(); l2 <- list()
for (i in me1){
  p1 <- phix(err=err1,me=i)
  l1$y <- p1 
  l1$x <- log(err1)
  l2[[i]] <- list(x=l1$x, y=l1$y) 
  }
names(l2) <- format(me1)

xn1 <- expression(paste("Ln (Error  ", delta,")"))
Hmisc::labcurve(curves=l2, pl=TRUE, xlab=xn1, ylab=yn1, method="on top", keys=me1, col=me1+1, cex=1.2 )
mtext(tit1, outer=TRUE, cex=2)

par <- p

#-------------------------------------------
# looks better with ggplot

err1 <- seq(from=0,to=4, by = 0.5)
me1 <- seq(1, 5, by=1) # mean errors of system
# me1 <- round( emdbook::lseq(1,10,4), 1) # alternative; log scale

m1 <- matrix(ncol=3,nrow=0); m2 <- matrix(ncol=3,nrow=0)
for (i in me1){
  p1 <- phix(err=err1,me=i)
  m2 <- cbind(err1,p1,i)
  m1 <- rbind(m1,m2)
  }
colnames(m1) <- c("Error","Prob~","Mean error")
df1 <- data.frame(m1)

leg1 <- expression(paste("Mean error  ",epsilon))
xn1 <- expression(paste("Error  ", delta))
yn1 <- expression(paste("Probability  ", phi))
tit1 <- expression(paste("Probability ",phi," of error ",delta, " occurring in system with mean error ", epsilon) )

g1 <- ggplot2::ggplot(data=df1) 
g1 <- g1 + ggplot2::geom_smooth(aes (x=df1[,1],y=df1[,2], color = factor(df1[,3]), fill = factor(df1[,3]) ), size=2 ) +
ggplot2::scale_fill_discrete( name=leg1, breaks= unique(df1[,3]), label = unique(df1[,3]) ) + 
ggplot2::scale_color_discrete( name=leg1, breaks= unique(df1[,3]), label = unique(df1[,3]) ) +
ggplot2::scale_y_continuous(name=yn1) +
ggplot2::scale_x_continuous(name=xn1)+
ggplot2::opts(	
	title = tit1, 
	axis.title.x = theme_text(face="bold", size=25),
	axis.text.x = theme_text(face="bold", size=15),
	axis.title.y = theme_text(face="bold", size=25),
	axis.text.y = theme_text(face="bold", size=15),
	plot.title = theme_text(face="bold", size=30),
	legend.title = theme_text(face="bold", size=25),
	legend.key.size = unit(2, 'lines'),
	legend.text = theme_text(face="bold", size=15)
	)

res <- list(g1) # hold plots

#-------------------------------------------------------
# same with log scale

err1 <- round( emdbook::lseq(10,200,100), 0) # mean errors of system; log scale
me1 <- round( emdbook::lseq(10,1000,5), 0) # mean errors of system; log scale
m1 <- matrix(ncol=3,nrow=0); m2 <- matrix(ncol=3,nrow=0)
for (i in me1){
  p1 <- phix(err=err1,me=i)
  m2 <- cbind(err1,p1,i)
  m1 <- rbind(m1,m2)
 }
df2 <- as.data.frame(m1)

xn1 <- expression(paste("Error ",delta," (", log[10], " scale)"))

g2 <- ggplot2::ggplot(data=df2) 
g2 <- g2 + ggplot2::geom_smooth(aes (x=df2[,1],y=df2[,2], color = factor(df2[,3]), fill = factor(df2[,3]) ), size=2 ) +
ggplot2::scale_fill_discrete( name=leg1, breaks= unique(df2[,3]), label = unique(df2[,3]) ) + 
ggplot2::scale_color_discrete( name=leg1, breaks= unique(df2[,3]), label = unique(df2[,3]) )+
ggplot2::scale_y_continuous(name=yn1) +
ggplot2::scale_x_log10(name=xn1)+
ggplot2::opts(	
	title = tit1, 
	axis.title.x = theme_text(face="bold", size=25),
	axis.text.x = theme_text(face="bold", size=15),
	axis.title.y = theme_text(face="bold", size=25),
	axis.text.y = theme_text(face="bold", size=15),
	plot.title = theme_text(face="bold", size=30),
	legend.title = theme_text(face="bold", size=25),
	legend.key.size = unit(2, 'lines'),
	legend.text = theme_text(face="bold", size=15)
	)

res <- list(res,g2)

#-----------------------------------------------------------
# psi formula from paper for required error
#-----------------------------------------------------------

psix <- function(x){
	a <- 2/sqrt(2*pi)
	err <- function(x) exp(-0.5*x^2)
	c <- pracma::quadinf(err,x,Inf) 
	psix <- a*c
	return(psix)
}

x1 <- seq(from=0,to=5, by = 0.25)
l1 <- as.double(length(x1))
psix1 <- vapply(FUN=psix, x1, FUN.VALUE=l1)
# me1 <- round( emdbook::lseq(1,10,4), 1) # alternative; log scale

m1 <- cbind(x1,psix1)
df3 <- as.data.frame(m1)

xn1 <- expression( paste(x, " = ", frac( paste("limit of error ",delta, " to reject ",k," observations") , paste("mean error ",epsilon))))
yn1 <- expression(paste("Probability  ", psi))
tit1 <- expression(paste("Probability ",psi," of error ",delta, " occurring in system with ratio ", x) )

g3 <- ggplot2::ggplot(data=df3) 
g3 <- g3 + ggplot2::geom_smooth(aes (x=df3[,1],y=df3[,2]), color="darkblue", size=2) +
ggplot2::scale_y_continuous(name=yn1) +
ggplot2::scale_x_continuous(name=xn1) +
ggplot2::opts(	
	title = tit1, 
	axis.title.x = theme_text(face="bold", size=25),
	axis.text.x = theme_text(face="bold", size=15),
	axis.title.y = theme_text(face="bold", size=25),
	axis.text.y = theme_text(face="bold", size=15),
	plot.title = theme_text(face="bold", size=30)
	)

res <- list(res,g3)

#------------------------------------------------
# compare psi function with erfc
#--------------------------------------------------

erfcR <- function (x) Re( erfc(x) ) # real value from erfc in NORMT3

x1 <- seq(from=0.01,to=3, by = 0.1)
l1 <- as.double(length(x1))
psix1 <- vapply(FUN=psix, x1, FUN.VALUE=l1)
erfc1 <- vapply(FUN=erfcR, x1, FUN.VALUE=l1)

gp1 <- rep(c(1,2),each=l1) # 1=psix, 2=erfc
x11 <- rep(x1,2)
m1 <- cbind(x11, c(psix1, erfc1), gp1)

df4 <- as.data.frame(m1)

xn1 <- expression( paste(x, " = ", frac( paste("limit of error ",delta, " to reject k observations") , paste("mean error ",epsilon))))
yn1 <- expression(paste("Probability  "))
tit1 <- expression(paste("Probability for values of ratio ", x , " from functions ", psi, " and erfc") )
leg1 <- "Function"

g4 <- ggplot2::ggplot(data=df4) 
g4 <- g4 + ggplot2::geom_smooth(aes (x=df4[,1],y=df4[,2], color = factor(df4[,3]), fill = factor(df4[,3]) ), size=2 ) +
ggplot2::scale_fill_discrete( name=leg1, breaks= c(1,2), labels = c( expression(psi), expression(erfc) )) +
ggplot2::scale_color_discrete( name=leg1, breaks= c(1,2), labels = c( expression(psi), expression(erfc) )) +
ggplot2::scale_y_continuous(name=yn1) +
ggplot2::scale_x_continuous(name=xn1)+
ggplot2::opts(	
	title = tit1, 
	axis.title.x = theme_text(face="bold", size=25),
	axis.text.x = theme_text(face="bold", size=15),
	axis.title.y = theme_text(face="bold", size=25),
	axis.text.y = theme_text(face="bold", size=15),
	plot.title = theme_text(face="bold", size=30),
	legend.title = theme_text(face="bold", size=25),
	legend.key.size = unit(2, 'lines'),
	legend.text = theme_text(face="bold", size=15)
	)

res <- list(res,g4)

#-----------------------------------------------
# Probability function from paper - attempts
#-----------------------------------------------

phix <- function(err,me){ # err=error, me=mean error; as above
	a <- 1/(me * sqrt(2*pi))
	b <- exp (-err^2 / (2*me^2))
	phi <- a*b
	return(phi)
	}

Prob1 <- function(x, meanerr,k){
	psix1 <- psix(x)
	requiredLimit <- meanerr * x
	phi1 <- phix( err=requiredLimit, me=meanerr )
 	prob <- (psix1/phi1)^k
	return(prob)
}

meanerr1 <- seq(from=0.8, to=1.2, by=0.1) # ratios
x1 <- seq(from=10, to=15, by = 5)
k1 <- seq(from=1,to=5, by=1)

m1 <- matrix(ncol=4,nrow=0) 
m2 <- matrix(ncol=4,nrow=0)
for (i in x1){
	for (j in meanerr1){
 p1 <- Prob1(i,j,k1)
 m2 <- cbind(i,j,k1,p1)
 m1 <- rbind(m1,m2)
}
}
colnames(m1) <- c("x=ratio","mean err~","k rejected","prob")
df5 <- as.data.frame(m1)
#print(df5)

# same expression; also taken from paper
Prob2 <- function (limit, meanerr, N, m, k){ # err = mean error of system
	np <- N-k
	x <- limit/ meanerr
	a <- 1/ ( (meanerr^np)*(2*pi)^(0.5*np) )
	b <- exp( 0.5* ( -N+m+ (k*x^2) ) )
	c <- (psix(x))^k	
	prob <- a*b*c
	return(prob)
}

limit1 <- seq(from=2, to=4, by=2)
meanerr1 <- seq(from=0.5, to=1.5, by=0.5)
N1 <- seq(from=10, to=16, by=2)
k1 <- seq(from=1,to=6, by=2)

m1 <- matrix(ncol=6,nrow=0) 
m2 <- matrix(ncol=6,nrow=0)

for (i in limit1){
	for (j in meanerr1){
		for(k in N1){
	p2 <- Prob2(i,j,k,m=1,k1)
	m2 <- cbind(i,j,k,1,k1,p2)
	m1 <- rbind(m1,m2)
	}
	}
}
colnames(m1) <- c("limit","mean err~","N","m","k rej~","prob")
df6 <- as.data.frame(m1)
#print(df6)

print(res)
}
