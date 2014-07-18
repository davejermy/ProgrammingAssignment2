## This script contains a pair of functions that compute and cache
## the inverse of a square matrix so that it can subsequently be returned
## without the inverse having to be recalculated.


## The makeCacheMatrix function creates a list object that can be used to cache 
## the inverse of a square matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { ## Used to change the matrix that is to be inverted
                x <<- y
                m <<- NULL ## Resets the cached inverse value to NULL
        }
        get <- function() x 
        setsolve <- function(solve) m <<- solve ## Caches the inverse value
        getsolve <- function() m                ## Returns the cached inverse value
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
        ## If cached solve value exists then return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()         ## Gets the matrix to be inverted
        m <- solve(data, ...)   ## Computes the inverse of the matrix
        x$setsolve(m)           ## Caches the inverse value
        m
}
