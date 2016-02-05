## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Function makeCacheMatrix  returns a list containing functions to 
# put a matrix x in the cache (set()), 
# get a matrix x out of the cache (get()),
# put the inverse of matrix x in the cache (setinv()),
# get the inverse matrix out of the cache (getinv()).
# Similiar with the makeVector() function proposed as an example.
makeCacheMatrix <- function(x = matrix()) {
  xi <- NULL # Let xi be the inversed matrix
  set <- function(y) {
    x <<- y
    xi <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xi <<- inv # let inv be the computed inverse of x, to be stored in xi
  getinv <- function() xi
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# As in the `cachemean` example, `cachesolve` calculates the inverse ix of matrix x 
# IF xi is not already in the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xi <- x$getinv() 
  # if xi already has a cached value, then it should be returned without computation
  if(!is.null(xi)) { 
    message("getting cached data")
    return(xi)
  }
  # if xi is NULL then xi has to be computed 
  data <- x$get() # let's get the data (the actual matrix) 
  xi <- solve(data, ...) # compute with the solve function
  x$setinv(xi) # and set the result in  the cache
  xi # and return it
}
