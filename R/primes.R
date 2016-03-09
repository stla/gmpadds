#' @name primes
#' @rdname primes
#' @title Prime numbers and products of prime numbers
#' @param n integer
#' @return \code{primes} returns the first \code{n} prime numbers, \code{primesprod} returns the first \code{n} products of the prime numbers
#' @examples
#' primes(5)
#' primesprod(5)
#'
NULL

#' @rdname primes
#' @export
primes <- function(n){
  out <- as.bigz(integer(n))
  out[1] <- nextprime(0)
  for(i in 2:n){
    out[i] <- nextprime(out[i-1])
  }
  return(as.vector(out))
}

#' @rdname primes
#' @export
primesprod <- function(n){
  return(cumprod.bigz(primes(n)))
}


#' Square-free integers
#'
#' Returns the first square-free integers
#' @export
#' @param n positive integer - ignored if \code{Nmax < Inf}
#' @param Nmax positive integer - ignored if \code{n < Inf}
#' @return the first \code{n} square-free integers or the first square-free integers lower than \code{Nmax}
#' @details A square-free integer is an integer which is divisible by no other perfect square than 1.
#' @examples
#' # the 15 first square-free integers:
#' squarefreenums(n=15)
#' # the square-free integers between 1 and 16:
#' squarefreenums(Nmax=16)
#' # this should go to 6/pi² when Nmax -> Inf:
#' Nmax <- 1000; length(squarefreenums(Nmax=Nmax))/Nmax
squarefreenums <- function(n, Nmax=Inf){
  if(is.finite(Nmax)) n <- Inf
  if(is.finite(n)) Nmax <- Inf
  if(n==Inf && Nmax==Inf) stop("Specify a finite n or a finite Nmax")
  n <- min(n,Nmax)
  out <- integer(n)
  j <- 1L
  out[j] <- 1L
  primes <- nextprime(0)
  prodprimes <- primes
  i <- 2L
  while(j < n){
    if(divides(i, prodprimes)){
      j <- j+1L
      out[j] <- i
      if(out[j]>Nmax) return(out[1:(j-1L)])
    }
    primes <- c(primes, nextprime(primes[i-1]))
    prodprimes <- prod(primes)
    i <- i+1L
  }
  return(out[1:j])
}
