## Together these functions implement a "matrix" that caches
## its own inverse for future use.

## This function wraps matrix data, providing getter
## and setter functions for caching purposes.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## this resets the stored inverse, and replaces the old x matrix
    ## with a new one
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    
    ## this is called by the cacheSolve() function to set the calculated
    ## inverse
    setinverse <- function(inverse) m <<- inverse
    
    ## and this gets the inverse if it has been set
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function checks the matrix object created above to
## see if its inverse is set, and sets the inverse if it is NULL.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' - 
    ## if this hasn't been set already it will return NULL
    m <- x$getinverse()
    
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    
    ## Note: ginv() in the MASS package provides a generalized matrix
    ## inversion which will work on more types of matrices than solve()
    ## (although ginv() makes some assumptions you might not want to)
    m <- solve(data, ...)
    x$setinverse(m)
    m  
}
