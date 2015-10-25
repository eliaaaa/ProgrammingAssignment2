
## The function makeCacheMatrix creates a list of functions, which can store a matrix and inverse of the matrix.
## The function cacheSolve solves inverse of a matrix and can get inverse from cache without solving again if the matrix has been solved already.

## The first function, makeCacheMatrix creates a special "matrix" object, which is a list containing a function to
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


## The following function takes the special "matrix" created by makeCacheMatrix function and returns the inverse of matrix in the special "vector".
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the matrix in the cache via the setinverse function.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached matrix")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
  
}
