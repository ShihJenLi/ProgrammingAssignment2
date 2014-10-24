## creat two functions - makeCasheMatrix() and cachesolve()
## makeCacheMatrix creates a special matrix object that can cache its inverse, and then cacheSolve 
## where cachesolve calculates the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(matrix) {
    x <<- matrix
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inverse_x <<-inverse
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. 
## Computing the inverse of a square matrix can be done with the solve function in R. 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inverse_x)
  } else {
    inverse_x <- solve(x$get())
    x$setinverse(inverse_x)
    return(inverse_x)
  }
}