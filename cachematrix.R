## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(y){
    x<<- y
    i<<- NULL
  }
  getMatrix<- function() x
  setInverse<- function(inverse) i<<- inverse
  getInverse<- function() i
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function

## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrix<- x$getMatrix()
  i<- solve(matrix ,...)
  x$setInverse(i)
  i
}
