## Put comments here that give an overall description of what your
## functions do

## create a list which contains a series of functions to set or get the matrix and its inverse
## the input parameter should be an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversem) inv <<- inversem
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## return the inverse of a matrix if it already exists or calculate the inverse if it does not exist
## the parameter of cacheSolve should be a list created by the above function makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  inv<-x$setinv(inv)
  inv## Return a matrix that is the inverse of 'x'
}
