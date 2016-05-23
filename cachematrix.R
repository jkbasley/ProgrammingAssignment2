## jkbasley Assignment 2 submission
## Functions for making a special matrix object that can cache its inverse then
## solve the inverse of the special matrix

## makeCacheMatrix creates a special matrix object that is capable of cacheing its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## this function computes the inverse of the special matrix created in
## makeCacheMatrix, if the inverse hasn't already been calculated

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
