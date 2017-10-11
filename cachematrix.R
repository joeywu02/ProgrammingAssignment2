## To avoid repeatedly calculate the inverse of a matrix, a time-consuming 
## process, these functions use the cache process to speed up.

## makeCacheMatrix() creates an R object that stores a matrix, the inverse of 
## the matrix, and four functions used in the cache process. "set" set the value 
## of the matrix. "get" get the value of the matrix. "setinv" set the value of
## the inverse of the matrix. "getinv" get the value of the inverse of the 
## matrix. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## CacheSolve() requires an matrix argument returned from makeCacheMatrix().
## When the function is called for the first time, inverse of the matrix is 
## calculated and cached. Subsequent calls of the function will return the
## inverse from previously stored cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
