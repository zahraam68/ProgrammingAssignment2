## Use lexical scoping to cache the inverse of a matrix
## without needing to recompute it 

## Creates a "makeCacheMatrix" object that stores a matrix and its inverse
## The object returns a list made up of four functions

makeCacheMatrix <- function(x = matrix()) {
              inv <- NULL
              
              set <- function(y){
                     y <<- x
                     inv <<- NULL
              }
              get <- function( ) x
              
              setinverse <- function(inverse) inv <<- inverse
              getinverse <- function ( ) inv
              
              list( set= set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Computes or retrieves the inverse of the matrix from "makeCacheMatrix"

cacheSolve <- function(x, ...) {
           inv <- x$getinverse( )
           if (!is.null(inv)) {
                  message("getting cached data")
                  return (inv)
           }
           data <- x$get()
           inv <- solve(data,...)
           x$setinverse(inv)
           inv
        ## Return a matrix that is the inverse of 'x'
}


