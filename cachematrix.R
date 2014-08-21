## The following functions calculate and cache the inverse of a matrix

## makeCacheMatrix creates an extended 'matrix' which can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    cacheInverse <- NULL
    
    set <- function(y) {
        x <<- y
        cacheInverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(inverse) {
        cacheInverse <<- inverse
    }
    
    getInverse <- function() {
        cacheInverse
    }
    
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve returns the inverse of a matrix. 
## It returns the inverse from the cache when exists.
## it computes and caches the inverse when the cache is still empty
## cacheSolve take makeCacheMatrix as argument

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        
        if (!is.null(inverse)){
            return (inverse);
        }
        
        matrixSource <- x$get()
        inverse <- solve(matrixSource)
        x$setInverse(inverse)
        
        ## Return a matrix that is the inverse of 'x'
        inverse 
}
