## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list of 4 functions
## cacheSolve returns the inverse of the matrix

## Write a short comment describing this function

## makeCacheMatrix creates a list of four functions i.e., set(), get() , setsolve() & getsolve()
## set() helps in setting the value of the matrix for which the inverse is to be calculated
## get() helps in retrieving the matrix for which inverse is to be calculated
## setsolve() is assigning the inverss of a matrix
## getsolve() is retrieving the inverse of the matrix

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


## Write a short comment describing this function

## This function returns the inverse of the matrix.
## It first checks whether the inverse has already been computed or not.
## If the inverse has already been computed it will return the inverse by retrieving its value from getsolve()
## If the inverse is not availabe i.e., if getsolve() is NULL, then it will compute the inverse 
## and assign the computed inverse to setsolve()

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
