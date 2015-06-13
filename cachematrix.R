
## makeCacheMatrix creates a special "matrix", which is a list containing functions to:
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the matrix inverse
##   - get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse to NULL
  i <- NULL
  
  # set the matrix to cache
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get the matrix
  get <- function() x
  
  # set the matrix inverse
  setinverse <- function(inv) i <<- inv
  
  # get the matrix inverse
  getinverse <- function() i
  
  # return the list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" (from makeCacheMatrix), however
## it checks to see if the inverse has already been calculated. If so, it skips the
## computation, otherwise it computes the inverse of the data and sets its cached value
cacheSolve <- function(x, ...) {
  # get the inverse of the "cached "special" matrix
  i <- x$getinverse()
  
  # if not null, return the cached value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # otherwise, get the matrix
  data <- x$get()
  
  # calculate the matrix inverse
  i <- solve(data)
  
  # set the inverse (to the cache)
  x$setinverse(i)
  
  # return the inverse
  i
}
