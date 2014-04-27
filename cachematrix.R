## makeCacheMatrix: creates a special matrix and caches the inverse.
## cahceSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix.

## Creates a special matrix, which is a list containing:
## set the value of the matrix
## get the value of the matrix
## set the value of the Inverse of matrix
## get the value of the Inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

     imtx <- NULL
     set <- function(y){
         x <<- y
         imtx <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) imtx <- inverse
     getInverse <- function() imtx
     
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Checks to to see if the inverse is already calculated. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Otherwise Inverse is calculated and invokes the setInverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  imtx <- x$getInverse()
  if(!is.null(imtx)){
     message("getting cached data")
     return(imtx)
  }
  
  data <- x$get() 
  imtx <- solve(data)
  x$setInverse(imtx)
  imtx
  
}
