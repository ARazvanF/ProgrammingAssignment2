
## This function creates a list with the get, set, getInv and setInv elements of a given matrix.

makeCacheMatrix <- function(A = matrix()) {
  inv <- NULL
  ## This function changes the given matrix to another one.
  set <- function(B=matrix()){
    A <<- B
    inv <<- NULL
  }
  ## This function shows the given matrix.
  get <- function(){
    A
  }
  ## This function sets the inverse matrix of the given one.
  setInv<-function(B=matrix()){
    inv <<- B
  }
  ## This function shows the inverse matrix of the given one.
  getInv<-function(){
    inv
  }
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}

## This function calculates the inverse function of a given one in the form of a list created with the previous function,
##or if it has been already calculated, it only looks for its value in the cache and returns it.

cacheSolve <- function(A, ...) {
  B <- A$getInv()
  if(!is.null(B)){
    message("getting cached data")
    return(B)
  }
  data <- A$get()
  B <- solve(data, ...)
  A$setInv(B)
  B
}
