## This file contains a pair of functions that calculate and cache the inverse of a matrix. 

## This function creates a matrix object that can be inversed

makeCacheMatrix <- function(x = matrix()) {
  
    matx_inv <- NULL
    set <- function(y) {
        x <<- y
        matx_inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) matx_inv <<- inv
    get_inv <- function() matx_inv
    list(set = set, get = get,
        set_inv = set_inv,
        get_inv = get_inv)

}


## This function checks whether the inverse of the matrix passed to it, exists in the cache.
## If the inverse exists in the cache, it is retrieved from the cache. Else the inverse is created,
## stored in the cache and returned

cacheSolve <- function(x, ...) {
    matx_inv <- x$get_inv()
    if(!is.null(matx_inv)) {
        message("getting cached data")
        return(matx_inv)
    }
    data <- x$get()
    matx_inv <- solve(data, ...)
    x$set_inv(matx_inv)
    
    ## Return a matrix that is the inverse of 'x'
    matx_inv
}
