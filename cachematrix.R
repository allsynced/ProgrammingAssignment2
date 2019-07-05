## below are a pair of functions that cache the inverse of a matrix

##This function creates a special "matrix" object that can cache its inverse
## by returning returns a list 
## of get, set of original matrix and inverse of set and get functions

## to run the code, install matlib package if it has not been installed. 

library(matlib)


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- inv(data, ...)
  x$setinv(i)
  i
}
