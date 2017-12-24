## These two functions are in partial fulfillment of the 
## R Programming Programming Assignment 2: Lexical Scoping.
##These two functions will cache the inverse of a matrix

## The function makeCacheMatrix creates a special matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  setmatrix <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) invmatrix <- inverse
  getinverse <- function() invmatrix
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the matrix 
## returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {
    message("getting cached invertible matrix")
    return(invmatrix)
  }
matrixdata <- x$getmatrix()
  invmatrix <- solve(matrixdata, ...)
  x$setinverse(invmatrix)
  return(invmatrix)
}

