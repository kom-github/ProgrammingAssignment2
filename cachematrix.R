## A pair of functions "makeCacheMatrix" and "cacheSolve" calculate and cache the inverse of a matrix.
## "makeCacheMatrix" creates a list containing a function to set and get the value of matrix and its inverse.
## "cacheSolve" calculates and cashes the inverse of a matrix or returns the cached value of inverse of a matrix if it was 
## already calculated

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by "makeCacheMatrix". 
## If the inverse has already been calculated (and the matrix has not changed), 
## then "cacheSolve" retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
