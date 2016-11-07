##   The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##   1- set the value of the matrix
##   2- get the value of the matrix
##   3- set the value of the solve (inverse)
##   4- get the value of the solve (inverse)
##
##
## This function creates a special "matrix" object that can cache its inverse. 
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## 
## Return a matrix that is the inverse of 'x'
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##
cacheSolve <- function(x, ...) {
   m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
