## This R program implements two functions used to compute the inverse of a matrix

## This function returns a list of functions to get,set,set the inverse and get the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL
  set <- function(y) {  #The setter function
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() {   #The getter function
    x
  }
  setinverse <- function(inverse_matrix) {  # Sets the inverse matrix
    inverse_m <<- inverse_matrix
  }
  getinverse <- function() {  # returns the inverse matrix
    inverse_m
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  #Returns the list of functions
}

## The following function calculates and returns the inverse of the special "matrix" created with the makeCacheMatrix function
cacheSolve <- function(x, ...) {  
  m <- x$getinverse()
  if(!is.null(m)) {   #Check for cached inverse
    message("getting cached data")
    return(m)
  }  
  data <- x$get()
  m <- solve(data)  #Calculates the inverse
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