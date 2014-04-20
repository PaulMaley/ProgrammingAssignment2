## Functions for creating a cached matrix and 
## its inverse - based on pattern provided. 

## Constructor function
## x: A matrix
## function returns an object (a list) containing
## a set of functions .. 

makeCacheMatrix <- function(x = matrix()) {
  ## We assume that the matrix is invertible !

  ## xi: local variable that will form part of the environment 
  ## of the environment for set(), get(), etc.. 
  ## x is an argument so is already defined in this environment
  xi <- NULL     ## Inverse matrix

  set <- function(y) {
    x <<- y      ## Contents of cached matrix may be changed after construction
    xi <<- NULL  ## if matrix is reset then cached inverse is no longer valid
  }
  get <- function() x
  setInverse <- function(inverse) xi <<- inverse
  getInverse <- function() xi
  
  ## return an object (list)
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Function which takes a CacheMatrix object as 
## created by the above function.
## It returns the inverse of the matrix either
## by solving the matrix or by returning a 
## previously calculated and cached version
## (Note that it returns a matrix not a CacheMatrix)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xi <- x$getInverse()
  
  if ( !is.null(xi) ) {
    message("Using cached version")
    return(xi) 
  }
  
  xi <- solve(x$get())
  x$setInverse(xi)
  xi
}


