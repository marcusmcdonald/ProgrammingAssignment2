## This file contains R functions that are 'able to cache potentially 
## time-consuming computations.' The makeCacheMatrix function and the cacheSolve
## function work together to reduce the need to repeatedly caclulate the inverse
## (using solve) on a matix when the matrix hasn't changed.

## makeCacheMatrix: This function creates a class-like matrix with functions to
##                  access and mutate the internal matrix object.

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


## cacheSolve: This function computes the inverse of the matrix held by the
##             makeCacheMatrix function. If the inverse has been caclulate and the
##             matix hasn't changed. cacheSolve simply returns the cached copy.

cacheSolve <- function(x, ...) {
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
