## Gives the user the inverse of a given matrix by calculating it and caching the result, or
## by looking for a cached result and retreiving it. 


## Creates functions for caching the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse of a given matrix 

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  
  inverse <- solve(x$get())
  x$setinv(inverse)
  inverse
}
