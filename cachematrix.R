## Caching Inverse of a Matrix


## The below function will creates a special Matrix
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

## This function computes the inverse of the Matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
  
}


# Example Usage
#  x <- matrix(c(1,2,1,4), nrow=2,ncol=2,byrow=TRUE)
#  y <-makeCacheMatrix(x)
#  cacheSolve(y)
#     [,1] [,2]
# [1,]  2.0 -1.0
# [2,] -0.5  0.5

 

