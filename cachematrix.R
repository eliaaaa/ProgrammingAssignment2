## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "vector", which is a list containing a function to
## 1.set the matrix
## 2.get the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix

## This function does not return anything.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function takes special "vector" created by makeCacheMatrix function and returns the inverse of matrix in the special "vector".
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached matrix")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  inverse
  
}
