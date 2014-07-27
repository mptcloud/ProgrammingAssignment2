## The following functions together serve to compute and return the inverse of a user-defined 
## matrix. Even though a 'solve' command exists for this purpose, the functions below work
## by caching matrices and their associated inverses thereby potentially saving some time 
## when the functions below are called repeatedly.

## makeCacheMatrix is a function which takes a matrix and returns a list of functions. 

makeCacheMatrix <- function(x = matrix()) {
        
## In the beginning, m, which holds the inverse of the matrix, is initialized to NULL in order
## to track whether the inverse has been computed previously and so prevent any unnecessary 
## calling of the 'solve' function in cacheSolve. set() sets the matrix up for computation of 
## its inverse, getinverse() returns the inverse, while setinverse() associates m with the 
## computed inverse of the matrix. This last function is used in cacheSolve.
        
        m <- NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x

        setinverse <- function(inverse) m <<- inverse

        getinverse <- function() m

        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve takes the cached matrix created in makeCacheMatrix and returns its inverse.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()

## In the following block of code, a check is performed on the variable m to see whether the 
## inverse has already been computed or not. If so, then the cached value is pulled up.

        if(!is.null(m)) {
                message("Getting cached inverse")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setinverse(m)
        
        m
        
}
