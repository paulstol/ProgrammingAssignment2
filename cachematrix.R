## These functions work together to provide a means to cache the inverse of
## a given matrix and to make that inverse available (from the cache) for
## subsequent inverse requests. If the inverse of a new matrix is requested,
## then a new inverse matrix is calulated and cached. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
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

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve retrieves the inverse 
## from the cache.

cacheSolve <- function(findInverseOfThisMatrix, ...) {
     inverseMatrix <- findInverseOfThisMatrix$getInverse()
     if(!is.null(inverseMatrix)) {
          message("Getting cached inverse matrix.")
          return(inverseMatrix)
     }
     else {
          message("Calculating inverse matrix.")
          baseMatrix <- findInverseOfThisMatrix$get()
          inverseMatrix <- solve(baseMatrix, ...)
          findInverseOfThisMatrix$setInverse(inverseMatrix)
          inverseMatrix
     }
}
