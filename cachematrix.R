## Put comments here that give an overall description of what your
## functions do

## The below function contains the sub-functions that cache a matrix

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) Inv <<- Inverse
        getInverse <- function() Inv
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## the below function gets the inverse of a given matrix and cache it using the
## MakeCacheMatrix functions

cacheSolve <- function(x, ...) {
        Inv <- x$getInverse()
        if(!is.null(Inv)) {
                message("getting the cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInverse(Inv)
        Inv
        ## Return a matrix that is the inverse of 'x'
}
