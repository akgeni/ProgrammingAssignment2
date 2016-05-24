## Put comments here that give an overall description of what your
## functions do



## returns the list containg functions to set and get data.
## once setInv is called it returned already calulated value.
## otherwise return NULL.
makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  
  get <- function() x
  setInv <- function(invMatrix) invMat <<- invMatrix
  getInv <- function() invMat
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


## It takes input as output of makeCacheMatrix function
## return cached inverse of matrix if already calculated
## otherwise solve itself.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInv()
  if(!is.null(invMat)) {
    message("returning cached inverse matrix")
    return(invMat)
  }
  data <- x$get()
  #print(data)
  invMat <- solve(data, ...)
  x$setInv(invMat)
  invMat
  
}
