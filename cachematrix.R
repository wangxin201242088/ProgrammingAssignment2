## makeCacheMa creates a special "vector", which is really 
##a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }##set the matrix
  get <- function() x ##get the matrix
  setinverse <- function(solve) inverse <<- solve##set the inversion of the matrix
  getinverse <- function() inverse##get the inversion of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }##check if there is the cached data of the inverse matrix, if so, show the
   ##cached data directly
  data <- x$get()##if not, get the matrix and solve it to get the inverse matrix.
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse ## Return a matrix that is the inverse of 'x'
}
