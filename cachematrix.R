## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
