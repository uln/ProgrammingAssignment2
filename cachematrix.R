## Functions for calculating and caching the inverse of a matrix.
## The input matrix has to be invertible

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(A = matrix()) {
    
    # Init variable that contains the inverted matrix
    invA <- NULL
    
    # Set the input matrix. Reset any previously calculated inverse
    set <- function(B) {
        A <<- B
        invA <<- NULL
    }
    
    # Get the currently stored input matrix
    get <- function()
        A
    
    # Set the inverse matrix
    setinv <- function(inv)
        invA <<- inv
    
    # Get currently stored inverse matrix
    getinv <- function()
        invA
    
    # Return a list with the implemented functions
    list(
        set = set, get = get,
        setinv = setinv,
        getinv = getinv
    )
    
}


## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated (and the
## matrix has not changed), then `cacheSolve` retrieve the inverse from
## the cache.

cacheSolve <- function(A, ...) {
    ## Return a matrix that is the inverse of 'A'
    
    # Read the cache - retun its value if not empty.
    invA <- A$getinv()
    if (!is.null(invA)) {
        message("getting cached data")
        return(invA)
    }
    
    # Calculate and store the inverse matrix if the cache is empty
    data <- A$get()
    invA <- solve(data, ...)
    A$setinv(invA)
    
    # Return inverse
    invA
    
}
