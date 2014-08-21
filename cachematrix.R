## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is composed of a list of 4 operations. This function is used to initialise and
## retrieve the matrix which inverse is to be calculated. It also allows to calculate and retrieve
## the matrix inverse.
## 1. setMatrix: which sets the values of the matrix which inverse is to be calculated.
## 2. getMatrix: which gets the current value of the matrix.
## 3. setMatrixInverse: which calculates the inverse of the matrix.
## 4. getMatrixInverse: which gets theinverse of the current matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function(){x}
  setMatrixInverse <- function(matrixinverse){m <<- matrixinverse}
  getMatrixInverse <- function(){m}
  list(setMatrix = setMatrix, getMatrix = getMatrix, setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## Write a short comment describing this function
## This function is used to retrieved the cached matrix inverse.
## If the matrix inverse has not been calculated yet the function
## calls the solve() method on the matrix. If the matrix inverse
## has already been calculated the value is retrieved from the "cached
## memory".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrixInverse()
  if (!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  dataMatrix <- x$getMatrix()
  matrixInverse <- solve(dataMatrix)
  x$setMatrixInverse(matrixInverse)
  matrixInverse
}
