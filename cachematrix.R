## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inverse matrix
  invMatrix <- NULL
  
  ##This updates the matrix and resets the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##get's the matrix
  get <- function() x
  
  ## stores the inverse, and gets the inverse if it exists, respectively
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## getting the inverse matrix if it exists
  invMatrix <- x$getInverse()
  if (!is.null(invMatrix)){
    message("getting cached data")
    return(invMatrix)
  }
  
  ##if the inverse doesn't exist, then get the matrix, invert it,
  ## set the inverted matrix as... well, the inverted matrix, and return that
  mat <- x$get()
  invMatrix <- solve(mat, ...)
  x$setInverse(invMatrix)
  invMatrix
  
}
