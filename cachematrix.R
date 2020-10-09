## Put comments here that give an overall description of what your
## functions do

## make a list of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix(NA,nrow=nrow(x),ncol=ncol(x))
  set <- function(y) {
    x <<- y
    inv <<- matrix(NA,nrow=nrow(x),ncol=ncol(x))
  }
  get <- function() x
  setinv <- function(inversem) inv <<- inversem
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## return the inverse if a matrix if already exist or calculate if not exist

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(all(!is.na(inv))) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  inv<-x$setinv(inv)
  inv## Return a matrix that is the inverse of 'x'
}
