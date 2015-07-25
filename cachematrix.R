## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix creates a sprecial "matrix" which is really a list of functions to maintain the cached value
##   1. set the value of the matrix and indicate that the matrix has changed (via set) since the last inverse was calculated
##   2. get the value of the matrix
##   3. set the value of the inverse and reset the changed state to false
##   4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    xChange <- TRUE
    set <- function(y) {
        x <<- y
        inverse <<- NULL
        xChange <<- TRUE
     }
    get <- function() x
    getChange <- function() xchange
    setinverse <- function(inv) {
      inverse <<- inv
      xchange <<- FALSE
    }
    getinverse <- function() inverse
    list(set = set, get = get, getChange = getChange, setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## This function calculates the inverse of the special "matrix" created above.  However, it first checks to see if the
##     inverse was already calculated and that the set function has not been called since the calculation.  If so, the 
##     function returns the cached value of the inverse.  Otherwise, it calculates the inverse using the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inverse <- x$getinverse()
    
    ## if not NULL and not changed
    if (!is.null(inverse) && !x$getChange()) {
      return (x$getinverse())
    }
    data <- x$get()
    
    # calculate inverse
    inv <- solve(data, ...)
    
    # cache value
    x$setinverse(inv)
    inv
}
