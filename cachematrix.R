## Coursera R Programming Course
## Assignment 2: Caching the Inverse of a Matrix

## Return a matrix that is capable of calculating and caching
## its inverse.

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return the inverse of the given cacheMatrix.

cacheSolve <- function(m, ...) {
    inv <- m$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- m$get()
    inv <- solve(data, ...)
    m$setinverse(inv)
    inv
}
