# 1. Matrix ------------------------------------------------------------------
## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
        # Set the matrix:
        m <- NULL
        set <- function(y) {
                # '<<-' assigns a value to an object in an environment that's
                # different from the current environment.
                x <<- y
                m <<- NULL
        }
        # Get the matrix:
        get <- function() x
        # Calculate and set the inverse:
        setinv <- function(inverse) m <<- inverse
        # Get the inverse:
        getinv <- function() m
        # list will be the input for the cacheSolve funtion
        list(set=set, get=get, setinv=setinv,getinv=getinv)
}
 

# 2. Cache Solve ----------------------------------------------------------

## Using the output of makeCacheMatrix() return the inverse

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        # check if inverse already calculated
        if(!is.null(m)){
                #if yes - get from cache and skip computation
                message("Great! Data was cached, yo!")
                return(m)
        }
        #if no - calculate the inverse
        m.data <- x$get()
        m <- solve(m.data, ...)
        # Set the value of the inverse in the cache by way of setinv:
        x$setinv(m)
        return(m)
}
