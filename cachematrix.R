
## MakeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## all af that using the function solve() of R. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}
