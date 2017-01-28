

## This function cretaes a list containing a function to set a Matrix, get a Matrix, set an Inverse Matix and get an Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invM <<- inverse
  getInverse <- function() invM
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the Inverse of the list created in the above function

cacheSolve <- function(x, ...) {
  invM <- x$getInverse  ()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  M <- x$get()
  invM <- solve(M, ...)
  x$setInverse (invM)
  invM
}
    
    
