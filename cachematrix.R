## These functions are for programming assignment#2 for the
## R Programming Course.

## This function creates a "matrix" that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function () inv
     list (set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve (data, ...)
     x$setinv(inv)
     inv
}
