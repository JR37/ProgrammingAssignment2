# Note -  I had a limited window to test this code, and I
# apologise in advance for this.

# This function is intended to find the inverse of a matrix and caches
# it, so it need not be computed unnecessarily later on

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##initiates variable
  set <- function(y) { ##intended to set variable outside the function environment to the global environment
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- solve(x) m <<- inverse_matrix ##finds the inverse of the matrix x, and passes it to a variable m, which is intended to be a global variable
  get_inverse <- function() m
    
}


# This function is intended to first check if the inverse
# of a matrix has been computed and cached. If it has, it 
# should use the cached inverse, otherwise it will calculate
# the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)) { ##checks that the value of m is not null
    message("getting cached data") ##if not null, proceeds with using cached inverse
    return(m)
  } ## if matrix inverse is not cached (i.e. m is null), proceeds with finding matrix inverse
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
  
}

