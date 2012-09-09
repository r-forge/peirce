libor <-
function(v){
	lv <- length(v)
	if (lv>18 | lv<6) return("Only applies to vectors of with 6-16 elements")
	rm1 <- NaN # number of elements to remove
	is.between <- function(x, a, b) {(x - a)  *  (b - x) > 0}
	if (is.between(lv,14.5,18.5) ) {rm1 = 4
		}else if (is.between(lv,10.5,14.5) ){rm1=3
		}else if (is.between(lv,7.5,10.5) ){rm1=2
		}else if (is.between(lv,5.5,7.5) ){rm1=1
	}else{rm1=NaN}
	v1 <- sort(v) 
	v1 <- v1[1:(lv-rm1)]
	v1 <- v1[-(1:rm1)]
	m1 <- mean(v1)
	return(m1)
	#res1 <- cbind(m1,2*rm1)
	#return(res1)
    }
