##The goal of these functions is to hide the time consuming computations such as calculating the inverse of
##a matrix in a large dataset.  Rather than repeating the calculation, the inverse of the matrix can be 
##looked up in the cache when needed. 
 

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse. It is a list containing function
##to set the value of a vector, get the value of the vector, and set and get the mean of the vector. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- mean
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function returns a matrix that is the inverse of 'x'.  It first checks to see if the 
##inverse has already been computed.  If not it computes the inverse and stores it in cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
        
}
