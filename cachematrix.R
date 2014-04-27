## Put comments here that give an overall description of what your
## functions do
library(MASS)


## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  ## Set up the cached inverse
  inv <- NULL
  ## Create the matix functions set, get, setinverse, getinverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  ## Make the functions part of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ## Get the cached inverse if possible
      inv <- x$getinverse()
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
      ## Get the data
      data <- x$get()
      ## Calculate the inverse
      inv <- ginv(data, ...)
      ## Store the inverse in the cache
      x$setinverse(inv)
      ## Return the inverse
      inv
}
