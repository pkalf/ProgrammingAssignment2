## This R program implements two functions used to compute the inverse of a matrix

## This function returns a list of functions to get,set,set the inverse and get the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL     #Initializing the inverse matrix
  set <- function(y) {  #The matrix setter function
    x <<- y             # <<- operator is used to set variables of another environment
    inverse_m <<- NULL
  }
  get <- function() {   #The matrix getter function
    x
  }
  setinverse <- function(inverse_matrix) {  # Sets the inverse matrix
    inverse_m <<- inverse_matrix
  }
  getinverse <- function() {  # Returns the inverse matrix
    inverse_m
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  #Returns a list of the above functions
}

## The following function calculates and returns the inverse of the special "matrix" created with the makeCacheMatrix function
cacheSolve <- function(x, ...) {  
  inv <- x$getinverse()    #Attempt retrieval from cache
  if(!is.null(inv)) {      #Check for cache availability
    message("getting cached data")
    return(inv)            #Return the inverse matrix
  }  
  data <- x$get()       
  inv <- solve(data, ...)  #Calculates the inverse
  x$setinverse(inv)        #Sets the cache
  inv                      #Return the inverse matrix
}

## Example - Uncomment to run
# initial_matrix <- matrix(c(1:4),2,2)
# special_matrix <- makeCacheMatrix(initial_matrix)
# print(special_matrix$get())
# inverse_matrix <- cacheSolve(special_matrix)  #Not cached yet
# print(inverse_matrix)
# inverse_matrix <- cacheSolve(special_matrix)  #From cache
# print(inverse_matrix)