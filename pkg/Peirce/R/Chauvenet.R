Chauvenet <-
function(v, loop=FALSE){
	#require(NORMT3) # for erfc
	erfcR <- function (x) as.real(erfc(x))
	fun1 <- function(x) x > 0.5
	
	z <- abs( (v - mean(v)) )/sd(v) # absolute value
	ez <- erfcR(z) 
	ezn <- length(v)*ez
		
	vt <- as.logical( vapply(ezn, FUN=fun1, FUN.VALUE=length(ezn)) ) #
	v <- v[vt]
	
	if (loop==TRUE){
		while (FALSE %in% vt){
			if (length(v)==2) {return(v)}
		z <- abs( (v - mean(v)) )/sd(v) 
		ez <- erfcR(z)
		ezn <- length(v)*ez
		vt <- as.logical( vapply(ezn, FUN=fun1, FUN.VALUE=length(ezn)) )
		v <- v[vt]
		} 
	}   
return(v)
}
