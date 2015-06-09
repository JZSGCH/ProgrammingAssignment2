## makeCacheMatrix and cacheSolve will allow users to cache an already computed 
## matrix inversion. 

## makeCacheMatrix creates a special "matrix" object from a matrix, that can cache its
##inverse.Set, get, setinverse and getinverse are the four functions listed in
## the makeCacheMatrix object.

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve will try to retrieve the cached value from the makeCacheMatrix object
## via its getinverse function and if successfull will inform the user that 
##it's getting the cached value. If no value can be retieved, value is computed via
## the solve function.


cacheSolve <- function(x,...){
  s<-x$getInverse()
  if(!is.null(s)) {
    message("getting cache solved matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setInverse(s)
  s
}
