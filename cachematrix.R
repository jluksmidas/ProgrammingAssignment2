## Put comments here that give an overall description of what your
## functions do
## The makeCacheMatrix function creates a "matrix" object that can cache it's inverse.
## The cacheSolve function performs the inverse computation if this has not already been done.
## If the inverse has already been computed, it retrieves the result from the cache.

## ----------------------------------------------
## Write a short comment describing this function
## The function makeCacheMatrix creates a special "matrix" object (actually a list)
## The function 'set' sets the value of the matrix and the function 'get' retrieves the value of the matrix
## 'setinverse' returns the inverse of the matrix
## 'getinverse' simply fetches the value for m
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve #invert matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## ----------------------------------------------
## Write a short comment describing this function
## This function calls the getinverse of x and assigns it to m.
## If m is not empty then it returns the cached value for m and skips the computation of the inverse
## If m is empty then it computes the value of the inverse in the cache by applying the setinverse function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)#invert matrix
  x$setinverse(m)
  m
  }
