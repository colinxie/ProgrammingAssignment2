

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  l <<- list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- l$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- l$get()
  m <- solve(x)
  l$setinv(m)
  m
}
