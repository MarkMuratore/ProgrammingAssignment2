## Computing the inverse of a matrix is a costly computation.
## In order to save time, the following functions will compute
## the inverse of a marix and cache the value to be used later. 
## Doing this will allow the inverse to be looke up in the cache
## instead of having to be recomputed.

## This function will create a mateix object that can cahce its
## own inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function wil compute the inverse of the matrix returened
## by makeCacheMatrix above. If the inverse has already been caluclated
## then cacheSolve wil retieve the inverse. If not, the inverse will be 
## computed.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
