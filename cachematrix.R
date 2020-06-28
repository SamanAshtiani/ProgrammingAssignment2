



## this function sets a matrix and can cache the inverse of this matrix. It returns a list containing four functions,
# namely, set_mat, get_mat, set_inv, get_inv, which set and get the matrix and its inverse, respectively. 
 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_mat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get_mat <- function() x
  set_inv <- function(inversed) inv <<- inversed
  get_inv <- function() inv
  list(set_mat = set_mat, get_mat = get_mat, set_inv = set_inv, get_inv = get_inv)
}

## This function computes the inverse of the matrix set by above function. Before computing the inverse, it checks whether the inverse is
# already computed. If yes, then it gets the inverse from the cache and skips the computation and returns the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get_mat()
  inv <- solve(mat, ...)
  x$set_inv(inv)
  inv
}
