## These two functions work together to cache and inverse a matrix, 
## in order to show lexical scoping in R

## This makeCacheMatrix function creates the matrix object 
## that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) z <<- inverse
  getInverse <- function() z
  list (set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## The cacheSolve function returns a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
  z <- x$getInverse()
  if(!is.null(z)) {
    message("Getting cached data...")
    return(z)
  }
  mat <- x$get()
  z <- solve(mat, ...)
  x$setInverse(z)
  z
}