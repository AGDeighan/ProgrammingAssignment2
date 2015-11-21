## makeCacheMatrix() creates a matrix and can cache the inverse of that matrix.
## cacheSolve() checks to see if the inverse matrix has already been solved and
## returns the cached inverse if it has and if the inverse has not already been
## solved cacheSolve() computes the inverse, caches it, and returns it.

## makeCacheMatrix() creates a list of functions that create and cache a matrix 
## (setmatrix), return the cached matrix (getmatrix), cache the inverse of the
## matrix (setinvmatrix), and return the cached inverse matrix (getinvmatrix). 
## As is probably evident, makeCacheMatrix was made by modifying Prof. Peng's
## makeVector example.

makeCacheMatrix <- function(x = matrix()) {
  inversem <- NULL
  setmatrix <- function(y) {
    x <<- y
    inversem <<- NULL
  }
  getmatrix <- function() x
  setinvmatrix <- function(invmatrix) inversem <<- invmatrix
  getinvmatrix <- function() inversem
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
  
}


## cacheSolve uses the getinvmatrix function of makeCacheMatrix to get the
## cached inverse matrix and if the value is not NULL it returns it. Otherwise
## cacheSolve gets the matrix using the getmatrix function of makeCacheMatrix
## and computes the inverse, which it then caches using the setinvmatrix
## function from makeCacheMatrix. cacheSolve was created by modifying Prof.
## Peng's cachemean example.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inversem <- x$getinvmatrix()
       if(!is.null(inversem)) {
              message("getting cached inverse matrix")
              return(inversem)
       }
       data <- x$getmatrix()
       inversem <- solve(data, ...)
       x$setinvmatrix(inversem)
       inversem
}
