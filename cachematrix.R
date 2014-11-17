## Put comments here that give an overall description of what your
## functions do

## This function creates a  matrix object.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ##i is initially set to null
  set <- function(y){ ##set the value of the matrix
    x <<- y
    i <<- NULL
  }
  get<-function() x  ##get the value of the matrix
  setinverse<-function(solve) i <<- solve  ##this is called by cacheSolve for the first access and stored the value
  getinverse<-function() i  ##this will return the cached value to cacheSolve on subsequent access
  list(set=set, get=get,  ##this is the list of the methods
       setinverse=setinverse,
       getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
  i <- x$getinverse()  ##set the getinverse value to i
  if(!is.null(i)){  ##check if the inverse is not null
    message("getting cached data") ##message to return for the cached value
    return(i)  ##return the inverse matrix
  }
  data <- x$get()  ##set the get value to data, if the inverse is null
  i <- solve(data, ...) ## use solve function to returns the inverse matrix 
  x$setinverse(i)  ## set the inverse matrix to setinverse 
  i  ##return the inverse matrix
}
