# Creates a object to store data and methods to access and set the data stored
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- matrix(y, nrow=2, ncol=2)
    s <<- NULL
  }
  get <- function() matrix(x, nrow=2, ncol=2)
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# Create a function that get a makeCacheMatrix object and cache the result of the matrix inverse.
# If the result is cached, it get it from the makeCacheMatrix object instantiated
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
