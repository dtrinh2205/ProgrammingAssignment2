## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get the value of the matrix
  get <- function() x
  
  #set and get the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get the inverse 
  m <- x$getinverse()
  
  #if inverse already existed, then get the cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if not, returns the output of the solve function
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
