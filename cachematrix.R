## Functions for creating a matrix with a performance optimization 
## when solving for the inverse of the matrix.
## 
## Users should use makeCacheMatrix to wrap an existing matrix whereby 
## the inverse of the matrix will be cached.  Calling cacheSolve on the
## wrapped matrix will cache the inverse after the first time.  Further 
## invocations of cacheSolve on the wrapped matrix will use the cached inverse.

## Creates a special matrix whose inverse is calcualted once then cached
## for future use.  The returned matrix is really a list containing functions to
## 1. set the value of the matrix (will reset the cached inverse to null)
## 2. get the value of the matrix
## 3. set the cached value of the inverse
## 4. get the cached value of the inverse (null until cacheSolve is called)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() { x }
    setinverse <- function(inv) { inverse <<- inv }
    getinverse <- function() { inverse }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Solves for the inverse of a matrix.
##
## Takes a wrapped matrix created by calling makeCacheMatrix.  The matrix must
## be invertable otherwise an error will be generated.
## 
## This method will inspect the wrapped matrix for a cached inverse.  If the 
## value is null it will proceed to compute the inverse using solve, cache
## the value in the wrapped matrix and return the inverse matrix.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    X <- x$get()
    inverse <- solve(X, ...)
    x$setinverse(inverse)
    inverse
}
