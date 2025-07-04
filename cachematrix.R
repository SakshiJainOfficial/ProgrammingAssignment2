## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cached inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y    # Update the matrix
    inv <<- NULL  # Reset the cached inverse since the matrix has changed
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse of the matrix
  getinverse <- function() inv
  
  # Return a list of functions that manage the matrix and its inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  # Try to retrieve the cached inverse
  inv <- x$getinverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  data <- x$get()  # Get the matrix from the special object
  inv <- solve(data, ...)  # Compute the inverse using solve()
  
  # Cache the computed inverse
  x$setinverse(inv)
  
  # Return the inverse
  inv
}

