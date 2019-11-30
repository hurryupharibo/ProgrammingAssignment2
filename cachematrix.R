## Two functions that cache the inverse of a matrix

## This function below produces a matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function below produces the inverse of the matrix object returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
      message("Retrieving cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i    
}
