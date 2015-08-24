## These functions enable you to invert matrices and cache the results for fast access
## avoiding rerunning a previously run operation.

## This function creates the inverted matrix and caches it in a variable that
## can be accessed from another environment using the << operator.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## This function checks the cache first, if it exists then it returns the value
## if not then it reruns the solve function to invert the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
