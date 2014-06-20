## A matrix structure that provides
## access to a cached matrix inverse.


## Creates the caching matrix structure.
## This consists of a list object containing
## functions for setting and getting the matrix
## and for setting and getting the matrix inverse
## The matrix and its inverse (initially null)
## are contained in the structure, but are
## only accessible via the getter and setter
## functions.
##
## NOTE: The matrix provided is assumed to be
## invertible.  No error will be returned if the
## provided matrix is singular, but calls to
## the 'cacheSolve()' function will fail.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
    setInv = setInv,
    getInv = getInv)
}


## This method will calculate the inverse for
## the cached-inverse matrix structure defined
## by the 'makeCacheMatrix()' function.
## If the matrix structure already contains 
## the calculated inverse, it will return
## this directly without doing further
## calculations.  If it does not contain
## the inverse, it will calculate it using
## the 'solve()' function.
##
## NOTE: This function assumes the provided matrix
## is invertible.  The function will fail if a
## non-invertible matrix is provided.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("Retrieving cached inverse.")
    return (inv)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInv(inverse)
  inverse
}
