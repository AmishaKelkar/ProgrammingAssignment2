# The goal for this  assigment is to first, write a function that will produce a matrix
  (makeCacheMatrix). Second will be to create another function that will inverse the first matrix
  (cacheSolve).
  
# makeCacheMatrix function will create aspecial "matrix" object that can 
  ## cache its inverse for the input (which is an invertible square matrix) 

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
      x <<- y
      a <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) a <<- inverse
    getinverse <- function() a
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
# cacheSolve is a function that will invert the matrix of (makeCacheMatrix) If the inverse is calculated 
    (and the matrix has not changed), then the cachesolve should retrieve the 
    inverse from the cache    
    
cacheSolve <- function(x, ...) {
          a <- x$getinverse()
          if(!is.null(m)) {
            message ("getting cached data")
            return (a)
          }
          data <- x$get()
          a <- inversw(data, ...)
          x$setinverse(a)
          a
}

