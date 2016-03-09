#' @name cumprod
#' @rdname cumprod
#' @title Cumulative product for big integers and big rationales
#' @param x \code{bigz} or \code{bigq} vector
#' @return  a vector whose elements are the cumulative products of the elements of \code{x}
#' @examples
#' cumprod(as.bigz(c(2, 3, 4)))
#' cumprod(as.bigq(c(2, 3, 4), 2))
NULL

#' @rdname cumprod
#' @export
cumprod <- function(x) UseMethod("cumprod")

#' @rdname cumprod
#' @export
cumprod.bigz <- function(x){
  out <- as.bigz(integer(length(x)))
  for(i in 1:length(x)) out[i] <- prod(x[1:i])
  if(is.null(dim(x))) out <- as.vector(out)
  return(out)
}

#' @rdname cumprod
#' @export
cumprod.bigq <- function(x){
  out <- as.bigq(integer(length(x)))
  for(i in 1:length(x)) out[i] <- prod(x[1:i])
  if(is.null(dim(x))) out <- as.vector(out)
  return(out)
}


#' Determine if k divides n
#'
#' Determine whether an integer \code{k} divides an integer \code{n}
#'
#' @export
#' @param k big integer
#' @param n big integer
#' @return \code{TRUE} or \code{FALSE} according to whether \code{k} divides \code{n}
#' @examples
#' divides(as.bigz(3), as.bigz(9))
divides <- function(k,n){
  as.bigz(n/k)*k == n
}
