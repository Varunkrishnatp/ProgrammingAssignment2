## These functions work together to cache the inverse of a matrix.
## This is useful when the inverse needs to be computed multiple times,
## saving time by avoiding redundant calculations.

## makeCacheMatrix creates a special matrix object that can store
## its own inverse once computed.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  ## Setter function to update the matrix and clear the cached inverse
  set <- function(y) {
    x <<- y          # Assign new matrix to x
    inv <<- NULL     # Reset inverse since the matrix has changed
  }
  
  ## Getter function to retrieve the matrix
  get <- function() x
  
  ## Setter function to cache the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## Getter function to retrieve the cached inverse (if available)
  getinverse <- function() inv
  
  ## Return a list of all functions so they can be accessed externally
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix created by makeCacheMatrix.
## If the inverse has already been calculated and the matrix hasn't changed,
## then the cached value is returned.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Try to get the cached inverse
  
  ## If the inverse is already cached, return it with a message
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If the inverse is not cached, compute it
  mat <- x$get()         # Get the matrix
  inv <- solve(mat, ...) # Compute the inverse using solve()
  
  x$setinverse(inv)      # Cache the inverse for future use
  inv                    # Return the computed inverse
}
