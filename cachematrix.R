
## Caching the Inverse of a Matrix:
## function below stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
##the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of 
##the inverse in the cache

cacheSolve <- function(x, ...) {

  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
