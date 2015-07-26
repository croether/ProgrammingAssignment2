# makeCacheMatrix and cacheSolve are a pair of functions that cache the inverse 
# of a matrix. 

## makeCacheMatrix() creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL            # initialise inverse matrix
    set <- function(y)  {
        x <<- y
        invM <<- NULL       # empty inverse in this envt
    }
    get <- function() x 
    setinvM <- function(inverse) invM <<- inverse
    getinvM <- function() invM 
    list(set = set, get = get,
         setinvM = setinvM,
         getinvM = getinvM)
}


# cacheSolve runs on the output of makeCacheMatrix and serves to invert an 
# (invertible) matrix x. If the inverse has already been calculated, then it 
# is retrieved from the cache. 

cacheSolve <- function(x, ...) {
    invM <- x$getinvM()
    if(!is.null(invM))  {       # retrieve inverse from cache if appropriate
        message("getting cached data")
        return(invM)
    }
    data <- x$get()             # else, invert matrix
    invM <- solve(data, ...)
    x$setinvM(invM)
    invM
}
