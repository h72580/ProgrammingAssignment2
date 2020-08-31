## The following functions, makeCacheMatrix and cacheSolve provide a "*possible*"
## solution for the problem of computational effort to obtain the inverse of
## a matrix.
## makeCacheMatrix creates a special type of "matrix" that can store the matrix
## data as well as it's inverse in order to be able to retrieve it instead of
## recalculating it.
## cacheSolve contains the logic to calculate the inverse of this special matrix
## and to save it in the matrix cache. subsequent call to the function will
## retrieve the inverse from the special matrix cache instead of recalculating
## it.

## This function creates a special "matrix" object that can cache its inverse.
## Input:
## x: the matrix containing the data to be saved in the cacheMatrix object.
##    if no input is provided an empty matrix will be used.
## Output:
## the cacheMatrix object initialized with the data in input
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## sets the matrix data and clean the inverse matrix cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## returns the data matrix
  get <- function() x
  ## sets the cache with the inverse data provided
  setinverse <- function(inverse) inv <<- inverse
  ## returns the previously cached inverse data, if any
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve
## the inverse from the cache.
## Input:
## x: the cacheMatrix object from which the inverse should be calculated
## ... other input to be passed onto the solve standard function
## Output: the inverse of the cacheMatrix provided in input
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## check if cacheMatrix x already contains a cached inverse matrix
  inv <- x$getinverse()
  if(!is.null(inv)) {
    return(inv)
  }
  
  ## get the data matrix from cacheMatrix x
  data <- x$get()
  
  ## calculate the inverse matrix with solve
  inv <- solve(data, ...)
  
  ## cache the inverse matrix calculated
  x$setinverse(inv)
  
  ## return the inverse matrix
  inv
}
