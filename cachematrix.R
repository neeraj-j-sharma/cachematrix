## overall, these two functions are used to calculate the inverse of the matrix.
## For the sake of computation-efficiency, caching of already calculated inverse 
## of the matrix is done in the program.


## this function can creates a 'matrix' object which is to be used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## this function firstly checks if the result or matrix to be obtained is same as the one
## already obtained, if yes, then cached data is retrieved. otherwise, inverse is obtained
## using solve method!

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
