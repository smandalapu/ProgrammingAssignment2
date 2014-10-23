## The functions allow you to create a matrix with a cacheable inverse
## assumes that the input matrix is a square invertable matrix
## To test:
## > source("cachematrix.R")
## > m<-matrix(1:4,ncol=2)
## > y<-makeCacheMatrix(m)
## > cacheSolve(y)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(y)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Note: The first time cacheSolve is invoked the inverse for matrix is cached 
## and the second call returns the inverse from cache

## Create the matrix that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function() m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of original matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  x$setinverse()
  x$getinverse()
}
