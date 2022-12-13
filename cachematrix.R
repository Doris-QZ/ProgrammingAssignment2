## The functions below are for caching the inverse of a matrix.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      invs <- NULL
      set <- function(y) {
            x <<- y
            invs <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) invs <<- inverse
      getinverse <- function() invs
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function returns a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
      invs <- x$getinverse()
      if(!is.null(invs)) {
            message("getting cached data.")
            return(invs)
      }
      data <- x$get()
      invs <- solve(data)
      x$setinverse(invs)
      invs
}

