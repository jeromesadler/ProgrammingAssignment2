##These utility functions create a special matrix object that can cache its inverse.

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  solvedMatrix <- NULL
  #get and set functions
  set <- function(y) {
    x <<- y
    solvedMatrix <<- NULL
  }
  get <- function() {
    x
  }
  #setSolve(), calls solve to return the inverse 
  setSolve <- function(solve) {
    solvedMatrix <<- solve
  }
  #getSolve(), returns the cached solution 
  getSolve <- function() {
    solvedMatrix
  }
  
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  solvedMatrix <- x$getSolve()
  if(!is.null(solvedMatrix)) {
    message("Info: Retrieving data from the cache")
    return(solvedMatrix)
  }
  #Nothing is cached for this matrix
  dat <- x$get()
  solvedMatrix <- solve(dat, ...)
  x$setSolve(solvedMatrix)
  # Return a matrix that is the inverse of 'x'
  solvedMatrix
}
