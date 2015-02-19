## This function creates a special "matrix" object

makeCacheMatrix <- function(mcx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mcx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mcx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special

cacheSolve <- function(mcx, ...) {
  inverse <- mcx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mcx$get()
    invserse <- solve(data, ...)
    mcx$setinv(inverse)
    return(inverse)     
}
