makeCacheMatrix <- function(x = matrix()) {
   # For a given matrix, creates a wrapped version that supports a cached
   # inverse matrix calculation value. This can be used to reduce the expense
   # of recomputing this value.
   #
   # Args:
   #   x (matrix) - matrix data (used as the basis for the cache).
   #
   # Returns:
   #   list - list of functions to get/set the matrix and it's inverse.
   #
   # Example:
   #   cm <- makeCacheMatrix(matrix(seq(1:4), 2, 2))

   m <- NULL

   # create setter/getter for underlying matrix
   set <- function(y) {
      # use the <<- operator to search into parent environments for the target
      # variable when this function is called; in this context, this behaves a
      # bit like a static variable reference because x, m will resolve to the
      # variables defined within this function.
      x <<- y
      # remove cached value, it will be recalculated on next cacheSolve call
      m <<- NULL
   }
   get <- function() x
   #
   # create setter/getter for inverse matrix
   setInverse <- function(inverse) m <<- inverse
   getInverse <- function() m
   
   # return list of functions to manipulate this 'object'
   list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
   # Return a matrix that is the inverse of the data stored in cache-matrix x.
   # If a cached inverse is available, it is returned, otherwise it is computed,
   # stored for future reference, and returned.
   #
   # Args:
   #   x (makeCacheMatrix result): value returned by prior makeCacheMatrix call.
   #   ...: additional function arguments to be passed to 'solve' call
   #
   # Returns:
   #   Matrix - Inverse of the matrix stored in the given cache-matrix.
   #
   # Example:
   #   cm <- makeCacheMatrix(matrix(seq(1:4), 2, 2))
   #   cacheSolve(cm)   # initial inverse calculation
   #   cacheSolve(cm)   # will retrieve inverse from cache
   
   # get and return existing (cached) value, if present
   xInverse <- x$getInverse()
   if (!is.null(xInverse)) {
      message("Getting cached data")
      return(xInverse)
   }
   # no cached value available, compute, store, and return it
   xData <- x$get()
   xInverse <- solve(xData, ...)
   x$setInverse(xInverse)
}
