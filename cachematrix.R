## This function is for the coursera assignment 2.

## This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y){
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv_x <<- solve
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the matrix from makeCacheMatrix. 
## If the inverse is already calculated, that it retreives the inverse from the cache.
  cacheSolve <- function(x, ...) {
  
## Return a matrix that is the inverse of 'x'
       inv_x <- x$getinverse()
       
##If inverse is already known, return the inverse
       if(!is.null(inv_x)) {
         message("getting cached data")
         return(inv_x)
       }
       
     data <- x$get()
     inv_x <- solve(data, ...) 
     x$setinverse(inv_x)
     inv_x
   }