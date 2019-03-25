## makeCacheMatrix creates a matrix with an inverse and stores the value of both
## cacheSolve retrieves the value of the inverse and recalculates it if necessary

## this function sets and gets both the values of the matrix and the inverse
## the <<- operators stored in the set function are what cache the values

makeCacheMatrix <- function(x = matrix()) {
  z<-NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <-function(x)
    setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this function checks for the value of the inverse of the matrix
## if z is null, it returns null
## it then retrieves the value of the matrix, solves for the inverse,
## stores it back in z, and prints z

cacheSolve <- function(x, ...) {
  z <-x$getinverse()
  if (!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data<-x$get()
  z<-solve(data)
  x$setinverse(z)
  z
}
