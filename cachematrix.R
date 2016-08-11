## This function creates a special object that inputs a
## square invertible matrix and caches its inverse.
makeCacheMatrix <- function(mat = matrix()) {
  
  ## Initialize a variable to hold inverse matrix
  invMat <- NULL
  
  ## set function sets the original matrix to mat
  set <- function(y) {
    mat <<- y
    invMat <<- NULL
  }
  
  ## get function gets the original matrix
  get <- function() mat
  
  ## setInverse sets an inverse matrix to invMat
  setInverse <- function(z) invMat <<- z
  
  ## getInverse gets the inverse matrix
  getInverse <- function() invMat
  
  ## Create a list of caching functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function retuns the inverse of a matrix that exists
## in a makeCacheMatrix object.  It will create the inverse
## if neeeded.
cacheSolve <- function(x, ...) {
  
  ## Get the inverse matrix
  invMat <- x$getInverse()
  
  ## If invMat is empty, get the matrix, invert it, and cache the result
  if (is.null(invMat)) {
    message("Inverse Matrix is not cached and will be calculated")
    mat <- x$get()
    invMat <- solve(mat, ...)
    x$setInverse(invMat)
  } else {
    message("Inverse Matrix is cached")
  }
  
  ## Return the inverse matrix
  invMat
}