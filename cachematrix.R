## The functions take a matrix as an input and return the inverse of that matrix
## and also stores the inverse in the cache. If the inverse of a matrix is present
## in the cache it will give the cached value else it will calculate and return
## the inverse of the matrix.

## The below function takes a matrix as an input and returns a list of functios
## ("getInverse","setInverse") to access the cache("m") as well as functions
##("get","set") to access the matrix("x") itself.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The below function takes the list("x") that was the output of makeCacheMatrix and
## returns the inverse("m") of the matrix that was passes to the makeCacheMatrix.
## If the inverse is present in the cache it returns the cached value else 
## calculates and returns the inverse. It also stores the inverse in the cache.

cacheSolve <- function(x) {
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
