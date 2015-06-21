## makeCacheMatrix is an object containing 4 funtions to store(1,3) and retreive(1,3) a matrix
## and it's inverse.  The values are stored in the object environment rather than the function's
##environment which allow all the functions to share the variables.
##you need to assign makeCacheMatrix to an object to use it: ie 
## x <-makeCacheMatrix(thisMatrix)
## makeCacheMatrix$set(thisMatrix)


makeCacheMatrix <- function(x = matrix()) {
##   First initialize v (which will store the inverse matrix) before we set it
     v <- NULL
##   $set() will store a new "primary" matrix in the object.  x will already have been set when
##   you first create an makeCacheMatrix object, this will change it and clear any cache 'v'
     set <- function(y) {
          x <<- y
          v <<- NULL
     }
## $get() simply returns the matrix stored with set
     get <- function() x
## $setinverse() simply stores a matrix passed to it, but cacheSolve will use this to store the inverse 
## which it will calculate
     setinverse <- function(inv) v <<- inv
## $getinverse() simply returns the matrix stored with set 
    getinverse <- function() v
## this definese the output of the function, which will now be a "list" of functions
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function receives a "makeCacheMatrx" object, collects a stored primary matrix and
## calculates the inverse with the solve() function.  It then stores the value in the same
## makeCacheMatrix.  If the inverse is already stored, it will skip the calculation at just
## return the cached inverse matrix

cacheSolve <- function(x, ...) {
## fist check x for a pre-existing cache
     v <- x$getinverse()
## if the inverse is not null, just return the stored value
     if(!is.null(v)) {
          message("getting cached data")
          return(v)
     }
## If we get here, then the inverse cache was NULL an we need to get the primary matrix and calc
     data <- x$get()
## Actually calculates the inverse
     v <- solve(data, ...)
## Stores the inverse in the cache of the makeCacheMatrix object
     x$setinverse(v)
## return & print the inverse matrix
     v
}
