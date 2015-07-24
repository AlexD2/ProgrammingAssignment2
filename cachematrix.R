## These 2 functions cache the inverse of a matrix so that it
## won't have to be computed repeatedly every time we need it.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
## Sets the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
## Gets the value of the matrix
    get <- function() x
## Sets the inverse of the matrix
    setsolve <- function(solve) m <<- solve
## Gets the inverse of the matrix
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Returns a matrix that is the inverse of 'x', unless there 
## already is a cached inverse (in which case it returns that
## already cached inverse)

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
## Checks if there is already a cached inverse
    if(!is.null(m)) {
## If there is a cached inverse, it informs you and returns it
        message("getting cached data")
        return(m)
    }
## Otherwise it finds the inverse, then returns it
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

