
## This function creates a special "matrix" object that can cache its inverse.
## For instance, it can set and get the original matrix and set and get the inverse matrix of it.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  
    ## set method will set i to a special token, and set x to a new value.  

    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## get method returns the matrix.
    get <- function() { x } 
    
    ## setInverse saves the results from chsheSolve function to i.
    setinverse <- function(inverse) { i <<- inverse }
    
    ## getInverse just returns the inverse matrix.
    getinverse <- function() { i }
    
    ## Returns a list with all of the functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, it gets the cached data.
## If the inverse has not been calculated, it calcuates the inverse matrix with solve() function.

cacheSolve <- function(x, ...) {
    
    ## retrieve the saved inverse matrix to i variable. 
    i <- x$getinverse()
    
    ## if there is no saved inverse matrix (NULL), then it print our a message
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## get the matrix and and save it to data variable.
    data <- x$get()
    
    ##Run solve function so that it can get inverse matrix.
    i <- solve(data, ...)
    
    ## Save the inverse matrix.
    x$setinverse(i)
    
    ## Return the inverse matrix.
    i
}
