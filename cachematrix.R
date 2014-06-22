# The "makeCacheMatrix" function creates a list which is a function to
# set the value of the matrix and
# get the value of the matrix and
# set the value of the inverse matrix and
# get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(mat) m <<- mat
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}

# The "cacheSolve" function calculates the inverse matrix from the matrix
# which is created with the "makeCacheMatrix" function. It first checks to see
# if the inverse matrix has already been calculated. If so, it gets the inverse
# matrix from the cache and skips the computation. Otherwise, it calculates
# the inverse matrix and cache it via the setmat function.
cacheSolve <- function(x, ...) {
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmat(m)
  m
}
