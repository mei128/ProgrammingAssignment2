## R Programming - Exercise 2
## Manuel Esteban-Infantes
##
## makeCacheMatrix
##
## Creates a cached matrix object - If no arguments are passed, default is an empty matrix (NA).
## 
## Upon creation, a null cache for the inverse matrix is created too
##
## The object has four methods which are nothing but setters and getters
## 
##    set         : sets the matrix and resets the cache of the inverse matrix
##    get         : returns the current matrix
##    setInverse  : stores the passed (inverse) matrix in the cache
##    getInverse  : returns the inverse matrix stored in the cache (or null)
##
## Each call to makeCacheMatrix creates a new environment where a matrix and its inverse are stored.
## The setter methods have to make sure that the passed values are assigned in this parent environment
## (the environment created each time makeCacheMatrix was called).

makeCacheMatrix <- function(x = matrix()) {
    ## create cache for the inverse, initially NULL
    minv <- NULL
  
    ## set : setter for a new matrix
    set  <- function(y) {
        x    <<- y      # set X in the parent environment
        minv <<- NULL   # reset the cache of inverse
    }
    
    ## get : getter for the current matrix - returns the matrix received as argument
    get <- function() x
  
    ## setInverse : stores the inverse in cache. Note assignment is in the parent environment
 
    setInverse <- function(inverse) minv <<- inverse
  
    ## getInverse : returns the cached inverse.
  
    getInverse <- function() minv
  
    ## return a list of handles for the getter and setter methods
  
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##
## cacheSolve
##
## Returns the inverse of the special matrix object received as argument. 
##
## It obtains the cached matrix first, if is null (not previously set) obtain its value and store it,
## otherwise use value from the cache (console message to verify it is pulling it from the cache).
##
## Caveat:  It does no error handling, so it assumes that the matrix was created with makeCacheMatrix,
##          and that it is invertible.
##
## NOTE It would have been better to build cacheSolve as part of the makeCacheMatrix$getInverse method,
##      as it is done in the makeCacheMatrixToo below.


cacheSolve <- function(x, ...) {
    ## get the cached inverse first
    minv <- x$getInverse()
    if(is.null(minv)) {
        ## if it is null
        data <- x$get()           # Obtain current matrix
        minv <- solve(data)       # Solve its inverse - we really do not need ... to solve the inverse
        x$setInverse(minv)        # and store it in the cache
    
    } else {
        ## if the cache was set signal that it using cached data  
        message("Using cached data")
    }
    ## Return the inverse
    minv
}

##
## makeCacheMatrixTwo - A better version of makeCacheMatrix, with very similar way of working.
##
##    The main difference is that the $getInverse method incorporates the cacheSolve funcionality:
##    the first time it's called it obtains and returns the inverse matrix, subsequent calls pull the
##    results out of the cache. The setInverse method is removed and the value set directly.
##

makeCacheMatrixToo <- function(x = matrix()) {
    ## create cache for the inverse, initially NULL
    minv <- NULL
  
    ## set : setter for a new matrix
    set  <- function(y) {
        x    <<- y      # set X in the parent environment
        minv <<- NULL   # reset the cache of inverse
    }
  
    ## get : getter for the current matrix - returns the matrix received as argument
    get <- function() x
  
    ## getInverse : obtains inverse if not set, otherwise returns the cached inverse.
  
    getInverse <- function() {
        if (is.null(minv))    minv <<- solve(x)
        else                  message("Using cached data")
        return(minv)
    }
  
    ## return a list of handles for the getter and setter methods
  
    list(set = set, get = get, getInverse = getInverse)
}
