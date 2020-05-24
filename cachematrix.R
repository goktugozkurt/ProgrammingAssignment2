## In order not to compute the matrix repeatedly, this functions creates and 
## caches the matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  g <- NULL
  set <- function(y) {
    x <<- y
    g <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) g <<- inverse
  getinverse <- function() g
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  g <- x$getinverse()
  if (!is.null(i)) {
    message("cached data")
    return(g)
  }
  data <- x$get()
  g <- solve(data, ...)
  x$setinverse(g)
  g
}
