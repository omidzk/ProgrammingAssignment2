## Our goal is to write functions that help us cache the result of costly
## operation of calculating invserse of matrices. In order to do so we introduce
## two functions makeCacheMatrix and cacheSolve.

## makeCacheMatrix is a function that makes a list of needed functions which are
## get: gets matrix
## set: set matrix to the given argument
## getinverse: gets inverse of matrix which is the value of variable inv
## setinverse: sets inverse of matrix (sets the value of variable inv)

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inv variable to NULL which is the default value since inverse
  # hasn't been computed yet.
  inv <- NULL
  
  # define set function which sets the matrix to input and
  # its inverse to NULL (not computed)
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # define get function which returns the matrix
  get <- function() {
      x 
  }
  
  # define setinverse function which sets inverse of matrix to a calculated value
  setinverse <- function(inverse) {
      inv <<- inverse
  }
  
  # define getinverse function which returns inverse of matrix
  getinverse <- function() {
      inv
  }
  
  # return defined functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that consumes result of function makeCacheMatrix
## it uses getinverse function to get inverse value and checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the
## data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    # get value of inverse via getinverse
    inv <- x$getinverse()
    
    # if inverse if not null (computed already) return its vlaue and skip
    # further computations
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    # get matrix in order to calculate inverse
    data <- x$get()
    
    # calculate the invserse and store its value for future use
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    # return inverse of matrix
    inv
}
