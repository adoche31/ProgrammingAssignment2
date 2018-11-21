## Put comments here that give an overall description of what your
## functions do

## This function makeCacheMatrix contains the method
## to save the matrix in cache, to verify whether it is 
## already calculated ect.

makeCacheMatrix <- function(x = matrix()) {
  mat<-NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setmat<- function(matrix) mat <<- matrix
  getmat <- function() mat
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}

## This function returns the inverse of entry matrix x.The function verifies 
## whether the matrix has already been calculated and therefore is saved in 
## the cache. If not, this function calculates the inverse matrix.

cacheSolve <- function(x, ...) {
  mat <- x$getmat()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setmat(mat)
  mat
}
