##
# cachematrix.R
# author: Fredrik Rönnvall
# date: 2014-07-20
#
# Description ####
# This file provides a basic CacheMatrix functionality, where you can
# create and store a matrix and its inverse easily
# It also provides a function for returning the cache if it exists,
# as well as calculating and storing it in the CacheMatrix if needed

##
# This function creates a CacheMatrix provided the matrix 'x'
# It will also provide functions to get/set the matrix it caches
# as well as functions to get/set the inverse of the same matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # Allow storing a new matrix in the CacheMatrix and reset the inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Get the stored matrix
  get <- function() x
  
  # Set the inverse
  setinverse <- function(i) inverse <<- i
  
  # Get the inverse
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##
# This function will calculate and store the inverse of the CacheMatrix 'x' if
# it does not exist
# Lastly it will return the inverse of the CacheMatrix
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  # If the inverse has not been set we need to calculate and store it
  if ( is.null(i) ){
    m <- x$get()
    i <- solve(m)
    x$setinverse(i)
  }
  
  return (i)
}
