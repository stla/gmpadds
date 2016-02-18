#' @name cfbigq
#' @rdname cfbigq
#' @title Continued fractions
#'
#' @examples
#' library(gmp)
#' cf2bigq(as.bigz(c(0,1,2,3)))
#' cf2bigq(c(0,1,2,3))
#' x <- as.bigq(7,10)
#' bigq2cf(x)
#' cf2bigq(bigq2cf(x))
#' bigq2cf(cf2bigq(c(0,1,2,3)))
#' x <- as.bigq(11, 9)
#' bigq2cf(x)
#' cf2bigq(bigq2cf(x))
NULL

#' @rdname cfbigq
#' @export
cf2bigq <- function(x) UseMethod("cf2bigq")

#' @rdname cfbigq
#' @export
bigq2cf  <- function(x) UseMethod("bigq2cf")

#' @rdname cfbigq
#' @export
cf2bigq.bigz <- function(cf){
  b <- as.bigq(rev(cf))
  x <- b[2]+1/b[1]
  for(i in 3:length(cf)){
    x <- b[i] + 1/x
  }
  return(x)
}

#' @rdname cfbigq
#' @export
cf2bigq.integer <- function(cf){
  cf2bigq.bigz(as.bigz(cf))
}

#' @rdname cfbigq
#' @export
cf2bigq.double <- function(cf){

  cf2bigq.bigz(as.bigz(cf))
}

#' @rdname cfbigq
#' @export
bigq2cf.bigq <- function(x, n=10L){
  xn <- x
  cf <- as.bigz(floor.bigq(xn))
  frac_x <- frac.bigq(x)
  i <- 1L
  while(i<n && frac_x!=0L){
    x <- 1L/frac_x
    xn <- c(xn, x)
    cf <- c(cf, as.bigz(floor.bigq(x)))
    frac_x <- frac.bigq(x)
    i <- i+1L
  }
  return(as.integer(cf))
}

#' @rdname cfbigq
#' @export
bigq2cf.double <- function(x, n=10L){
  bigq2cf.bigq(as.bigq(x), n=n)
}

