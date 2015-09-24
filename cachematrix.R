## These two functions will take a special matrix and
## cache the inverse of the matrix.
## The purpose of these functions is to compute an inverse(or any value) of a matrix
##(in this example) more efficiently by caching value if it is not changing as it
## goes through the loop.

## makeCacheMatrix - Function that creates a special matrix, that is actually a list
## Function will set and get value of the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    ## <<- assigns a value from the current environment 
    ##to an object from a different environment
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve - Function that calculates the inverse of the special matrix above
## If it already has been calculated, it gets it from the cache, then it shows the matrix. 
## Else it calculates the matrix, stores it via the cache, then shows it.

cacheSolve <- function(x, ...){
  ## Returning a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Getting Cached Data")
    return(inverse)
  }
  data <- x$get()
  inverse <- inv(data, ...)
  x$setinverse(inverse)
  inverse
}

## Sources: I used the templates given in the example on the Programming 
## Assignment 2 Instructions page.
