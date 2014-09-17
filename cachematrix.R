## Usage and Testing:
## x <- matrix(sample(1:9), nrow=3) # Create a 3x3 matrix filled with random int from 1 to 9
## y <- makeCacheMatrix(x) # Use our function to create a special matrix
## all(x == y$get()) # Retrieve the matrix in our special matrix and compare it against x. It should return TRUE.
## cacheSolve(y) # Solve for the inverse of our special matrix
## cacheSolve(y) # Call it again to see if the data is cached. We should see "getting cached data".
## all(solve(x) == y$getinverse()) # Retrieve the inverse of our special matrix and compare it against the inverse of x. It should return TRUE.

## makeCacheMatrix is a function that stores the matrix in a variable x,
## and then returns a list of functions (set, get, setinverse, getinverse),
## which can then be used to set or get the value of the matrix, or set or
## get the value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  # Store the inverse matrix in i
  i <- NULL
  
  # Set the value of the matrix. The inverse is set to NULL whenever the
  # value of the matrix is set.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Set the value of the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  
  # Get the value of the inverse of the matrix
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a function that returns the inverse of the matrix 'x'.
## If the inverse already exists (cached), it will return the cached
## result. Else, it will calculate the inverse, cache it, and then return
## the result.
cacheSolve <- function(x, ...) {
  # Gets the value of the inverse (may be NULL)
  i <- x$getinverse()
  
  # If inverse is already calculated (NOT NULL), return the value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # If inverse is not calculated yet (NULL), we solve it
  data <- x$get()
  i <- solve(data, ...)
  
  # Cache the inverse of the matrix
  x$setinverse(i)
  
  # Return the result of the inverse of the matrix
  i
}
