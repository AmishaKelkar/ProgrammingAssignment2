# The goal for this  assigment is to first, write a function that will produce a matrix
  (makeCacheMatrix). Second will be to create another function that will inverse the first matrix
  (cacheSolve).
  
# makeCacheMatrix function will create aspecial "matrix" object that can 
  ## cache its inverse for the input (which is an invertible square matrix) 

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
    
# cacheSolve is a function that will invert the matrix of (makeCacheMatrix) If the inverse is calculated 
    (and the matrix has not changed), then the cachesolve should retrieve the 
    inverse from the cache    
    
cacheSolve <- function(x, ...) {
          m <- x$getmean()
          if(!is.null(m)) {
            message ("getting cached data")
            return (m)
          }
          data <- x$get()
          m <- mean(data, ...)
          x$setmean(m)
          m
}

