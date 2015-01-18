## Functions two enable caching of the inverse of a matrix
## and getting the inverse from the cache to avoid costly
## recalculations

## Defines a matrix object capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## set function that clears the cache
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## simply returns the matrix data
    get <- function() x
    
    ## sets the inverse to be cached
    setInverse <- function(calculatedInverse) inverse <<- calculatedInverse
    
    ## gets the cached inverse
    getInverse <- function() inverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse of a matrix either from the
## cache if present or freshly calculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    
    ## If there is a cached inverse we can return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## since no cache was found, we have to calculate,
    ## set it as the cached value and finally return it
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}
