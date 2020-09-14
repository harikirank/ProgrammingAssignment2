## Caching the matrix

## creates a matrix which will then be cached
## so that inverse will not be calculated every time

makeCacheMatrix <- function(x = matrix()) {
  # 1.create four functions : setMatrix, getMatrix, setInverse, getInverse
  invMatrix <<- NULL
  setMatrix <- function(mat) {
    x <<- mat
    invMatrix <<- NULL
    }
  getMatrix <- function() x
  setInverse <- function(mat) invMatrix <<- mat
  getInverse <- function() invMatrix
  
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
}


## stores the inverse of the matrix in a cache
## so that it can be retrieved later

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # 2.
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    print('getting cached data')
    return(inverse)
  }
  data <- x$getMatrix()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
