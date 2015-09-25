## TODO
## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(baseMatrix = matrix()) {
     inverseMatrix <- NULL
     set <- function(newMatrix) {
          baseMatrix <<- newMatrix
          inverseMatrix <<- NULL
     }
     get <- function() baseMatrix
     setInverse <- function(newInverseMatrix) inverseMatrix <<- newInverseMatrix
     getInverse <- function() inverseMatrix
     list(set = set, 
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inverseMatrix <- x$getInverse()
     if(!is.null(inverseMatrix)) {
          message("getting cached inverse matrix")
          return(inverseMatrix)
     }
     data <- x$get()
     inverseMatrix <- solve(data, ...)
     x$setInverse(inverseMatrix)
     inverseMatrix
}
