## The belwo functions calculate the Inverse of a matrix
## and stores it in the cache, so if the submitted matrix has it is Inverse
## value already calculated, then the function will get the value from cache.
## Otherwise, it will compute the value and store it in the cache

## The below function contains the sub-functions that are necessary 
## to cache a matrix

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
## MakeCacheMatrix functions, if the matrix is already submitted, the function
## will get the inverse from the cache. Otherwise, it will compute it and store
## it in the cache.

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
