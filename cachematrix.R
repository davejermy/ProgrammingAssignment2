## This script contains a pair of functions that compute and cache
## the inverse of a square matrix so that it can subsequently be returned
## without the inverse having to be recalculated.


## The makeCacheMatrix function creates an object that can be used to cache 
## the inverse of a square matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve function will return the inverse of the object created by
## the makeCacheMatrix function.  If the inverse has already been calculated
## (and cached) and the matrix has not changed, then it will be returned from
## the cache, otherwise it will be recalculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
