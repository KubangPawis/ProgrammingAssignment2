# PROGRAMMING ASSIGNMENT 2 - R Programming Course
# by KubangPawis

# ____________________________

# makeCacheMatrix(): this function creates a modified matrix object (different from a normal matrix) that 
# caches its inverse based on the function cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(non_inverse) {
    x <<- non_inverse
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(non_inverse) inverse <<- non_inverse
  get_inverse <- function() inverse
  
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}

# ____________________________

# cacheSolve(): this function is the main function that computes the inverse of the passed matrix

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  
  if (!is.null(inverse)) {
    message("You have already cached an inverse for this matrix...")
    return(inverse)
  }
  
  # If there are no cached inversed matrix, compute the inverse of the current matrix
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$set_inverse(inverse)
  inverse
}
