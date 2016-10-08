## The functions are analogous to the makeVector and cachemean examples, and
## are used to cache the value of a Matrix and calculate its inverse, or
## retrive the inverse from the cache if previously calculated.

## Caches the value of the matrix passed, and returns a list of functions 
## for setting and retrieving the matrix value and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) inv <<- mean
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Checks if the inverse has previously been calculated. If so, returns the
## inverse. If not, calculates it and returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
