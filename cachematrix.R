## name: makeCacheMatrix
## params: x, the matrix data
## returns: a list of functions that allow to matrix with cache
##
## description: The function returns an especial kind of matrix that store the value of it inverse
##
matrix  calculates the inverse of a matrix.  creates a special "vector" that includes a list g a function to

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
       )
}


## name: cacheSolve
## params: x, the CacheMatrix data
## returns: The inverse of a Matrix
##
## description: The function calculate the inverse a matrix. If execution of the inverse has been calculated
##              previously the function returns the value cached in the previous execution.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

