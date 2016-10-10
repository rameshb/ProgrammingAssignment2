## Since matrix inversion is expensive, here we have functions for creating a special matrix 
## whose inverse can be cached. A function that leverages the cache while solving for a matrix
## is also included. (solves only when inverse is not cached) 

## Creates a special matrix for which the inverse can be cached
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(inputMatrix) {
        x <<- inputMatrix
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse
    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Solves (computes inverse) a given special matrix created using makeCacheMatrix if 
## the inverse is not cached already. If a cached inverse exists, the cached value 
## is returned
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    inputMatrix <- x$get()
    invertedMatrix <- solve(inputMatrix)
    x$setInverse(invertedMatrix)
    invertedMatrix        
}
