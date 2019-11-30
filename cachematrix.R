## Two functions that cache the inverse of a matrix

## This function below produces a matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function below produces the inverse of the matrix object returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
      message("Retrieving cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i    
}
