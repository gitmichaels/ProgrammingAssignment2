## Functions do the following 
## 1) set up data structure that stores a matrix and its inverse,
##    provided the inverse has already been computed
## 2) compute matrix inverses, grabbing previously computed and stored
##    inverses, if available


## First function, makeCacheMatrix, sets up a list structure containing
##   matrix data, along with stored inverse, if already computed

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve takes list as structured by makeCacheMatrix,
##   and returns either the cached inverse, if available, or computes 
##   the inverse and stores the result

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {  ## Checks if cached inverse is available
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
