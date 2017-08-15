## The following two functions first create a special object that stores a matrix and caches its inverse.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function returns the inverse of the matrix (after first checking if
## the inverse has already been computed.) If it has, it gets the result and skips the
## computation. If not, it computes the inverse, then caches this value via setinverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Return a matrix that is the inverse of 'x'
## Below is a sample calculation
x = rbind (c(1, 2), c(2, 1))
m = makeCacheMatrix(x)
m$get()
m$getinverse() ##This matrix is the inverse of x
