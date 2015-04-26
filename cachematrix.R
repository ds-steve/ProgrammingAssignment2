## These functions support efficiently computing the
## inverse of a matrix by solving each inverse only
## once and caching the result. Each matrix is assumed
## to be invertible.

## creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## attempts to resolve a cacheMatrix's inverse from the
## cache created in `makeCacheMatrix`. If the inverse has
## not been computed, then it computes the inverse and
## caches the result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
