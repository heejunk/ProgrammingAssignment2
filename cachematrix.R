## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This function creates a special "matrix" object that can cache its inverse.
## For instance, it can set and get the original matrix and set and get the inverse matrix of it.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() { x } 
    setinverse <- function(inverse) { i <<- inverse }
    getinverse <- function() { i }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, it gets the cached data.
## If the inverse has not been calculated, it calcuates the inverse matrix with solve() function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
