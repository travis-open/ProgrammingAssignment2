## Functions compute and store the inverse of a matrix "x"
## 

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<-y
    m <<- NULL
  }
  get <- function()x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## computes the inverse of the matrix returned by the above function, unless it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
      
  }
  data <- x$get()
  m <- solve(data,...)
  x$setmatrix(m)
  m
}

