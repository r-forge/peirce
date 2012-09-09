PeirceVsChauvenet <-
function (width=1366, height=768) {

# require(outliers)
# require(ggplot2)
# require(TeachingDemos)
# require(compositions)
# source("Peirce.R")
# source("Chauvenet.R")


#--------------------------------------------
# Peirce vs Chauvenet, 4x examples
#--------------------------------------------

# Dummy to trick R CMD check
Ross <- NULL; rm(Ross)
NIST <- NULL; rm(NIST)
sa.outliers1 <- NULL; rm(sa.outliers1)

# examples from Ross paper
data(Ross, package="Peirce")
v1 <- Ross
# example from NIST handbook
data(NIST, package="Peirce")
v2 <- NIST
# example from TeachingDemos
data(outliers, package="TeachingDemos")
v3 <- outliers 
tryCatch(rm("outliers"), warning=function(w) w)
# data from compositions
data(SimulatedAmounts, package="compositions")
v4 <- as.vector(sa.outliers1)
rm(list=ls(pattern=glob2rx("sa.*")))

l1 <- list(v1,v2,v3,v4)
len1 <- sapply(l1, length)

p1 <- lapply(l1, FUN=Peirce)
len2 <- sapply(p1, length)

c1 <- lapply(l1, Chauvenet)
len3 <- sapply(c1, length)

cloop <- function(v) Chauvenet(v,loop=TRUE)

c1 <- lapply(l1, cloop)
len4 <- sapply(c1, length)

rem1 <- matrix(ncol=3, nrow=4, c(len1-len2, len1-len3, len1-len4)) # no. removed by each method
colnames(rem1) <- c("Peirce", "Chauvenet", "Chauvenet (repeated)")
rownames(rem1) <- c("Ross","NIST","TeachingDemos","compositions")
	

#--------------------------------
# set up graphical parameters
#--------------------------------

windows(record=T, width=width, height=height) # default ratio is 1.778


# plot them
p <- par

par(mfrow=c(2,2), mar=c(2,5,3,3), oma=c(1,1,2,1))
lapply(l1, plot)
mtext("Outlier datasets, values by index", outer=TRUE, cex=2)
lapply(l1, boxplot)
mtext("Outlier datasets, boxplots", outer=TRUE, cex=2)
lapply(l1, hist)
mtext("Outlier datasets, histograms", outer=TRUE, cex=2)
lapply(l1, qqnorm)
mtext("Outlier datasets, quantile-quantile plots", outer=TRUE, cex=2)

par <- p


#-------------------------------------------------------------------
# looks better with ggplot2


df1 <- as.data.frame(v1)
names(df1) <- "v"

tit1 <- "Example from Ross paper"

g1 <- ggplot2::ggplot(data=df1, aes(x=v)) +
ggplot2::geom_histogram(fill="darkblue",aes(y=..density..) ) +
ggplot2::geom_density(fill="lightblue", alpha=0.3, linetype=1) +
ggplot2::scale_y_continuous(name="") +
ggplot2::scale_x_continuous(name="") +
ggplot2::opts(	
	title = tit1, 
	axis.text.x = theme_text(face="bold", size=15),
	axis.text.y = theme_text(face="bold", size=15),
	plot.title = theme_text(face="bold", size=25)
	)

df2 <- as.data.frame(v2)
names(df2) <- "v"
df3 <- as.data.frame(v3)
names(df3) <- "v"
df4 <- as.data.frame(v4)
names(df4) <- "v"

tit1 <- "Example from NIST handbook"
g2 <- g1 %+% df2 # analogy function for ggplot objects
g2 <- g2 + ggplot2::opts(title=tit1)
tit1 <- "Outliers data from TeachingDemos"
g3 <- g1 %+% df3
g3 <- g3 + ggplot2::opts(title=tit1)
tit1 <- "Outliers data from compositions"
g4 <- g1 %+% df4
g4 <- g4 + ggplot2::opts(title=tit1)

gE1 <- gridExtra::grid.arrange(g1,g2,g3,g4, ncol=2, 
	main=textGrob("Datasets with outliers \n (Histograms with density plots)", 
		gp=gpar(font=2, cex=2) ) 
	)

return(rem1)
}