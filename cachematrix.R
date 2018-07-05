##This program will take a matrix and calculated its inverse matrix. 
##If the inverse value is the same as the previous calculated value,
##the program will track down the result from cache. Otherwise, the
##program will calcuated its inverse and display it on the screen. 

## makeCacheMatrix requires a matrix from user, and store it for the 
## next inverse function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL           
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseValue) m <<- inverseValue
  getInverse <- function() m
  
  list(set=set, get=get, 
       setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve function will take the matrix for the previous function, 
## and check if the inverse already existed in cache. If so function 
## will display the result rightway. If not, function will calcalate
## the inverse and dispaly the result. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)}
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}
