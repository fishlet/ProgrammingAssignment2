## this file contains two functions: makeCacheMatrix & cacheSolve
## see respective comments for more details

## compute and return a special matrix containing getters and setters 
## for both the input matrix and its cached inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function() x
  setMatrixInverse <- function(k) inverse <<- k
  getMatrixInverse <- function() inverse
  matrix(data = c(getMatrix, setMatrix, getMatrixInverse, setMatrixInverse), 
         nrow = 2, ncol = 2, 
         byrow = TRUE, 
         dimnames = list( c("matrix", "inverse"), c("getter", "setter") )
  )
}


## takes the output of the function makeCacheMatrix and:
## check if the cached matrix inverse exists
##    if yes: print log and return the cached matrix inverse
##    if not: get the original matrix data, compute its inverse, then cache and return the inverse
cacheSolve <- function(x, ...) {
  inverse <- x[["inverse", "getter"]]()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x[["matrix", "getter"]]()
  inverse <- solve(data)
  x[["inverse", "setter"]](inverse)
  inverse
}
