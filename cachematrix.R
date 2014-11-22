## This code presents two functions that allows to create matrixes
## with cacheable inverses, so they can be used to prevent from calculating
## matrix inverses multiple times, wich can be too expensive.

## makeChacheMatrix creates a special "matrix" with a cacheable inverse,
## it returns a list with 4 functions: set (sets the matrix data), 
## get (returns the matrix data), setinverse(sets the matrix inverse),
## getinverse (returns the cached inverse of the matrix), 
## setinverse (sets the inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of a "matrix" x that has to be the output
## a makeCacheMatrix function call. If the inverse had already been calculated
## for x, it returns the cached result, otherwise it will calculate it using
## the solve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
