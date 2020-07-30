## Provides a way to cache the inverse of a matrix. Useful for large matrices,
## where the calculation of the inverse is expensive.

## Creates a matrix wrapper that can cache its inverse. The inverse must be
## calculated using the cacheSolve function.
## INPUT: Matrix whose inverse will be cached
## OUTPUT: Wrapped matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse for a matrix wrapped by the makeCacheMatrix function.
## If the matrix did not change and the inverse was already calculated, returns
## the cached version. Otherwise, calculates, caches the result and returns it.
## INPUT: Matrix wrapped by makeCacheMatrix and other arguments to the solve
## function
## OUTPUT: Inverse of the matrix

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
