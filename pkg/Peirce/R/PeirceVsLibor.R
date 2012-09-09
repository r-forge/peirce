PeirceVsLibor <-
function(width=1366, height=768){
# source("libor.R")
# require (RColorBrewer)
# require(ggplot2)

#--------------------------------
# set up graphical parameters
#--------------------------------

windows(record=T, width=width, height=height) #ratio is 1.778

#----------------------------------------
# Peirce vs LIBOR example
#----------------------------------------


liborP <- function(v){
	lv <- length(v)
	v1 <- Peirce(v)
	lv1 <- length(v1)
	m1 <- mean(v1)
	res1 <- cbind(m1,lv-lv1)
	#return(res1) # use this to get no. outliers eliminated also
	return(m1)
}

data("liborUSD3M", package="Peirce")
df5 <- liborUSD3M

isavg <- rep(0.8, nrow(df5))
meth <- rep(0, nrow(df5))

df5 <- cbind(df5, isavg)
df5 <- cbind(df5, meth)

u1 <- unique(df5$Date)
for (i in 1:length(u1) ) {
	
	v1 <- subset(df5, df5$Date == u1[i] , select = X3M)
    r1 <- libor(as.double(v1$X3M))
	r1 <- cbind(" Libor average",u1[i],r1,0.9,0)
	colnames(r1) <- colnames(df5)
 	df5 <- rbind(df5,r1)
	
	r2 <- liborP(as.double(v1$X3M))
	r2 <- cbind(" Peirce average",u1[i],r2,0.9,1)
	colnames(r2) <- colnames(df5)
	df5 <- rbind(df5,r2)

}

df5 <- transform(df5, Date = as.Date(Date, format="%m/%d/%Y"))
df5 <- transform(df5, X3M = as.numeric(X3M))
df5 <- transform(df5, Bank = as.factor(Bank))
df5 <- transform(df5, isavg = as.numeric(isavg))
df5 <- transform(df5, meth = as.factor(meth))

#df2 <- subset(df5, isavg==1) # look at averages only

# choose colours
vals1 <- RColorBrewer::brewer.pal(9, "Blues")
vals2 <- RColorBrewer::brewer.pal(9, "Greens")
vals3 <- c(vals1,vals2)
vals3[1] <- "#A020F0"
vals3[2] <- "#FFA500"
vals4 <- alpha(vals3,0.5)

# this is useful in picking them
GetColorHexAndDecimal <- function(color)
{
  c <- grDevices::col2rgb(color)
  sprintf("#%02X%02X%02X %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3])
}


tit1 <- "Interest rate submissions and averages \n for Libor (US Dollars, 3 month rate)"
 
g5 <- ggplot2::ggplot(data=df5, aes(x=Date, y=X3M, group=Bank))
g5 <- g5 + ggplot2::geom_line( aes(group=Bank, colour=Bank, size=I(isavg))) +
ggplot2::scale_colour_manual(values = vals4) +
ggplot2::scale_area(name="Line represents \n average value ?", breaks=c(0,0.9), labels=c("No - thin lines", "Yes - thick lines") ) +
ggplot2::scale_y_continuous(name="Interest \n rate (%)") +
ggplot2::scale_x_date(name="Date (2008)") +
ggplot2::opts(	
	title = tit1, 
	axis.title.x = theme_text(face="bold", size=15),
	axis.text.x = theme_text(face="bold", size=10),
	axis.title.y = theme_text(face="bold", size=15),
	axis.text.y = theme_text(face="bold", size=10),
	plot.title = theme_text(face="bold", size=20),
	legend.title = theme_text(face="bold", size=10),
	legend.text = theme_text(face="bold", size=10)
	)

# use this to check how many eliminated each day

df5 <- df5[df5$isavg==0.8,] # remove averages

u1 <- unique(df5$Date)
nrem1 <- vector(length=length(u1)) # to hold no. removed each day
for (i in 1:length(u1) ) {
	v1 <- subset(df5, df5$Date == u1[i] , select = X3M)
	r1 <- Peirce(as.double(v1$X3M)) 
	r2 <- 16-length(r1)
	nrem1[i] <- r2
}
res <- data.frame(u1,nrem1)
colnames(res) <- c("Date","No. removed (Peirce)") 

plot(res, col="darkblue", cex=2, 
	main="No. outliers removed each day \n Red - by LIBOR method, Blue - by Peirce method",	
	ylab="No. removed",
	xlab="Date (2008)")
points(x=res[,1],y=rep(8,nrow(res)),col="red",cex=1.5)

print(g5)
return(res)

}
