## Below are 2 functions, makeCacheMatrix and cacheSolve. Together they are useful in calculating
## the mean of an invertible matrix, either using its cached value, or using matrix inversion, 
## depending on the situation.

# makeCacheMatrix takes a numeric input, assigns it to x, sets its inverse, denoted by m, to NULL
# It returns a list of 4 functions: set, get, setinv and getinv. It is possible to access
# these 4 functions directly and individually for various purposes if needed
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  
  
  # In this function, the matrix is getting initialized with new data, so its inverse is reset to NULL  
  set <- function(y) {    
    x <<- y   # The <<- is used because x and m are needed for use outside of this function 
    m <<- NULL
  }
  
  # "get" simply returns the value of the matrix at any point of time
  get <- function() x
  
  # setinv takes a user input value called inv and assigns it to m, which is the inverse, thereby
  # bypassing the need to calculate it
  setinv <- function(inv) m <<- inv
  
  # getinv simply returns the value of the matrix inverse at any point of time
  getinv <- function() m
  
  # this is the output of the function - a list of 4 functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve takes as its input a list denoted by x and retrieves the value of the inverse, denoted
# by "m", using getinv. If m is not null, then that is the inverse matrix, and is returned. If m is
# null, then the function gets the data using $get and computes its inverse using solve,  
# saves it to m, and returns the inverse, m
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}