NlogQLimits <-
function(){

# source(NlogQ.R)

# empiric limits
N1 <- seq(from=3,to=145, by = 1)

m1 <- matrix(ncol=3,nrow=0); m2 <- matrix(ncol=3,nrow=0)

for (i in N1){
 k1 <- seq(from=1, to=i-2, by=1) # reject up to N-2 outliers	
 NLQf <- function(k2) NlogQ(k=k2, N=i) # allow function to vectorize
 NLQfE <- function (x) {  # wrapper for R1 which returns NA instead of error message
  return(tryCatch(NLQ(x), error=function(err) NA))
  }
 NLQ1 <- vapply(k1, FUN=NLQf, FUN.VALUE=NaN) 
 m2 <- cbind(i,k1,NLQ1)
 m1 <- rbind(m1,m2)
}

nacounts <- apply(m1, 1, function(x)length(x[is.na(x)]))
m2 <- m1[nacounts/ncol(m1) < 0.1,] # remove values of NA
colnames(m2) <- c("N","k","NlogQ")
df1 <- as.data.frame(m2)
len1 <- nrow(df1)
tit1 <- "NlogQ for values of N and k \n \n"

#require(rgl)

rgl::bg3d("white")
rgl::plot3d(x=df1$N, z=df1$k, y=df1$NlogQ, col=rainbow(len1),
	xlab="", zlab="", ylab="",main="",sub=" ")
rgl::par3d(cex=2)
rgl::decorate3d(xlab="    N", zlab="            k", ylab="")
rgl::text3d(x=50, texts="NlogQ", adj=0.7)
rgl::bbox3d(color=c("#333377","black"), emission="#333377",specular="#3333FF", shininess=5, alpha=0.8)

#rgl::snapshot3d(filename="NlQrgl.png")
}
