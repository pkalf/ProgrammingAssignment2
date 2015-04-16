## This R program implements two functions used to compute the inverse of a matrix

## This function returns a list of functions to get,set,set the inverse and get the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL
  set <- function(y) {
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) inverse_m <<- inverse_matrix
  getinverse <- function() inverse_m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}

## The following function calculates the inverse of the special "matrix" created with the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m  
}

## Example - Uncomment to run
# initial_matrix <- matrix(c(1:4),2,2)
# special_matrix <- makeCacheMatrix(initial_matrix)
# print(special_matrix$get())
# inverse_matrix <- cacheSolve(special_matrix)  #Not cached yet
# print(inverse_matrix)
# inverse_matrix <- cacheSolve(special_matrix)  #From cache
# print(inverse_matrix)