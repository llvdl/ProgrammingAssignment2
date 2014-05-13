## Functions to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
## using the cacheSolve function
##
## x is a matrix that defaults to an empty matrix.
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() { x }
    setinverse <- function(inverse) { cachedInverse <<- inverse }
    getinverse <- function() { cachedInverse }
    hasinverse <- function() { !is.null(cachedInverse) }
    
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse, 
         hasinverse=hasinverse)
}

## Returns a matrix that is the inverse of 'x'
##
## x is a special "matrix" object, as returned by the makeCacheMatrix function.
##
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    if(x$hasinverse()) {
        message("getting cached data")
        return(x$getinverse())
    } else {
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    }
}
