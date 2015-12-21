## The makeCacheMatrix function will create a list containing functions that
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the matrix inverse
## 4. Get the value of the matrix inverse
## We cache both the matrix inverse and the matrix values inside the makeCacheMatrix closure
## Note: Since we are utilizing the method closure we must null the stored matrix Inverse when a new value is set
## Or it's possible to return inverses to a different matrix than the one in question

makeCacheMatrix <- function(matrix) {
  matrixInverse <- NULL
  
  setMatrix <- function(matrixToSet) {
    matrix <<- matrixToSet
    matrixInverse <<- NULL
  }
  
  getMatrix <- function() matrix
  
  setMatrixInverse <- function(matrixInverseToSet) matrixInverse <<- matrixInverseToSet
  
  getMatrixInverse <- function() matrixInverse
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## The cacheSolve function will take a matrix value and check to see if the closure of the makeCacheMatrix method
## contains a value for the matrix inverse. If it does, then that value is returned. If it does not,
## Then the inverse is computer and set inside the makeCacheMatrix closure

cacheSolve <- function(cacheMatrix) {
  matrixInverse <- cacheMatrix$getMatrixInverse()
  
  if(!is.null(matrixInverse)) {
    message("getting cached matrix inverse")
    return(matrixInverse)
  }
  
  matrix <- cacheMatrix$getMatrix()
  matrixInverse <- solve(matrix)
  cacheMatrix$setMatrixInverse(matrixInverse)
  
  matrixInverse
}
