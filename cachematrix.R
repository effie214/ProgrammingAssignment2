

makeCacheMatrix <- function(x= matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } # Set the matrix 
  get <- function() x # Get the matrix
  setinverse <- function(inverse) m <<- inverse # Set the inverse of the matrix
  getinverse <- function() m # Get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}




cacheSolve <- function(x, ...) {
  
  
  m <- x$getinverse() # Get the inverse of the matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }             # If it has the inverse already, return the inverse
  data <- x$get() # If it doesn't have, solve the inverse and return it
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}