#' @name floorfrac
#' @rdname floorfrac
#' @title Integer part and fractionial part of a big rational
#' @import gmp
#'
#' @examples
#' library(gmp)
#' x <- as.bigq(9,2)
#' floor(x)
#' frac(x)
#' x - frac(x)
#' x <- as.bigq(9,13)
#' frac(x) == x
#' x <- as.bigq(c(3, 6, 7), 5)
#' floor(x)
#' frac(x)
#' ( x <- matrix(c(x,x+1L), ncol=2) )
#' floor(x)
#' frac(x)
NULL

#' @rdname floorfrac
#' @export
floor <- function(x) UseMethod("floor")

#' @rdname floorfrac
#' @export
frac <- function(x) UseMethod("frac")

#' @rdname floorfrac
#' @export
floor.bigq <- function(x){
  as.bigz(x)
}

#' @rdname floorfrac
#' @export
floor.double <- function(x){
  base::floor(x)
}

#' @rdname floorfrac
#' @export
frac.bigq <- function(x){ # pour x bigq >0
  return(x - as.bigz(x)/1L)
}


