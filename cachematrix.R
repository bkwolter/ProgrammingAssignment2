## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix() takes an invertible matrix and calculates the
## inverse of that matrix and storing it in memory.
## cacheSolve() returns the inverse of an invertibe matrix if it is already stored in cache,
## otherwise, it calculates and returns the inverse of the supplied matrix

## Write a short comment describing this function
## This function takes an invertible matrix and calculates its invers
## the result is stored in cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    y <- NULL
    
    setmatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inverse) {
        m <<- inverse
    }
    getinverse <- function() m
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function returns the inverse of an invertible matrix if it is 
## stored in cache.  Otherwise, it calculates its inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
            message("getting cached data")
            return(m)
    }
    data <- x$getmatrix()
    x$setmatrix(data)
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
