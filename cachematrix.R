## Functions for caching the inverse of a matrix
## and for solving the inverse of a matrix

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  setmatrix <- function(newmat) {
    mat <<- newmat
    inverse <<- NULL
  }
  getmatrix <- function() mat
  setinverse <- function(newinverse) inverse <<- newinverse
  getinverse <- function() inverse
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the inverse of a "matrix" returned by makeCacheMatrix above.
## If the inverse has been calculated already, use the cached inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getmatrix()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
