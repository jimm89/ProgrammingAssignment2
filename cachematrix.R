## The below functions cache the inverse of an input matrix and then call the inverse.

## This first function, makeCacheMatrix creates a list of functions. These functions:
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. solves for the value of the inverse of the matrix (setInverse)
## 4. gets the value of the inverse of the matrix (getInverse)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This second function checks to see if the inverse of this matrix is in the cache already.
## If so, it prints the inverse of the matrix.
## If not, it calculates the inverse of the matrix, and then prints it.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
