## Using the following functions, the inverse of an inputted matrix can be
## calculated, then printed as well as cached for retrieval. If a matrix is
## inputted for which the inverse is already calculated, the cached inverse
## will simply be printed with no redundant calculation.


## The "makeCacheMatrix" function initializes a list of functions to set and
## retrieve the input matrix as well as to set and retrieve its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # The cached inverse of a matrix
  inv <- NULL
  
  # Caches a given inputted matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Retrieves a saved matrix
  get <- function() x
  
  # Caches the inverse of an inputted matrix to the "inv" variable
  setinv <- function(inverse) inv <<- inverse
  
  # Retrieves the cached inverse of an inputted matrix provided it has been
  # set previously
  getinv <- function() inv
  
  # Returns a list of above defined functions to the parent environment
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The "cacheSolve" function takes the special matrix created by the
## "makeCacheMatrix" function and either returns the cached inverse for a
## given matrix or calculates and caches the inverse for future retrieval.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # Checks if the inverse matrix to the input is in cache; if so, returns the
  # inverse matrix in cache without any calculation
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculates the inverse of the inputted matrix and sets the
  # result in the list initialized in the "makeCacheMatrix" function
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  # Returns the calculated inverse matrix
  inv
}