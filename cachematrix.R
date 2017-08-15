## This is a collection of functionas that cache a matrix and its inverse
## To calculate an matrix invers you must use the cacheSolve function on

## The following function will create a cached matrix object
## The cache holds the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Returns a matrix that is the inverse of of a cached matrix 'x'

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    message("calculating inverse")
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
    inv
}
