## Caching the Inverse of a Matrix 
## The pair of functions below provides matrix inversion caching functionality:
## - matrix inversion is usually a costly computation operation
## - being able to take advantage of cached previous results may greatly
##   enhance performance in computing loops


## Creates a list object that implements a "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<-NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list( set = set, get = get, 
          setinv = setinv,
          getinv = getinv )
}


## Computes the inverse of the "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}