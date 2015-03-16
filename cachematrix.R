## This package contains functions to create a matrix-like object that
## wraps a normal R matrix and can cache its inverse. The extended matrix
## is implemented as a list containing functions to get and set its data
## and inverse.
##
## Examples:
##
## x <- makeCacheMatrix( matrix(c(1:4), 2, 2) )
## cacheSolve(x)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
##
## set.seed(123)
## x <- makeCacheMatrix( matrix(sample(3000*3000), 3000, 3000) )
##
## # First call will calculate inverse with solve
## system.time( cacheSolve(x) )
##   user  system elapsed
##  7.254   0.130   7.384
##
## # Second call will use cached value
## system.time( cacheSolve(x) )
##   user  system elapsed
##      0       0       0



## Create an extended matrix that can store a cached inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    ## Setting a new value for the matrix clears the cached inverse.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x

    ## Use cacheSolve instead of directly accessing the inverse
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Return a matrix that is the inverse of the extended matrix 'x'
## 'x' must be a matrix created with makeCacheMatrix or a compatible object.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    ## Return the cached inverse if it has been previously calculated
    if(!is.null(i)) return(i)

    ## Solve the inverse and store it inside 'x'
    i <- solve(x$get(), ...)
    x$setinverse(i)
    i
}
