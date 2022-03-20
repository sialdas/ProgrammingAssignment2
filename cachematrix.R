## These functions create a special matrix object and cache the inverse of this matrix.
 
## Declaring the variable inverse that we want to obtain and then the set and get functions
## for the matrix and for the calculated inverse, and generate a list with these functions.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(computedinverse) inverse <<- computedinverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Acquiring the value of the variable inverse and evaluating if it has any value, 
## if it does not have computing the inverse of the data is done with the
## solve function and return a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

