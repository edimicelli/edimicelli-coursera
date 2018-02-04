## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list object that stores 4 functions:
## set, get, setinverse, and getinverse.  These functions will, 
## respectively, set a matrix input, retrieve a previously cached
## matrix, set the value of the inverse of said matrix, or retrieve
## a previously cached value of the inverse of said matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix, but checks for a
## cached answer before calculating for the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                messaged("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
