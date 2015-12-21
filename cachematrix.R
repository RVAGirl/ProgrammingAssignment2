## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function defines the four functions that will be used to return the 
## inverse of a matrix that has been cached.

makeCacheMatrix <- function(x = matrix()) {
  inverted<-NULL
  set <- function(y){
    x<<-y
    inverted<<-NULL
  }
  get<-function() x
  setinverted<-function(solve) inverted<<-solve
  getinverted<-function() inverted
  list(set=set, get=get, setinverted=setinverted, getinverted=getinverted)
}
  
## cacheSolve calculates the inverse of a provided invertible matrix which has been cached or 
## calculates the inverse of an invertible matrix if it has not been cached.  The first time
## through, it will calculate the value but the remaining calls should return the previously
## inverted matrix.
## INSTRUCTIONS: Return a matrix that is the inverse of 'x' using function solve(x) and 
## assume it is always a square invertible matrix for this exercise.
cacheSolve <- function(x, ...) {
    inverted<-x$getinverted()
    if(!is.null(inverted)){
      message("getting cached data")
      return(inverted)
    }
      originMatrix<-x$get()
      inverted<-solve(originMatrix,...)
      x$setinverted(inverted)
      inverted
}


