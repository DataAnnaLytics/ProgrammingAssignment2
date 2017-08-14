## "makeCacheMatrix" creates a special "matrix-list" out of any matrix 
## which is a parameter for "cacheSolve". "cacheSolve" returns the inverse
## of the initial matrix but tries to avoid unnecessary re-calculation of
## the inversion by checking if the inverse was already calculated before.

## works analog to the provided example-function "makeVector":
## "makeCacheMatrix" creates a "matrix-list" containing several functions, 
## e.g. caching the inverse of a matrix if it was calculated before:

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y ##caches matrix
            i <<- NULL
      }
      get <- function() x ##returns matrix
      setinv <- function(Inverse) i <<- Inverse ##caches inverse
      getinv <- function() i ##returns inverse
      list(set = set, get = get,
               setinv= setinv,
               getinv = getinv)
}

## works analog to the provided example-function "cachemean":
## Returns the inverse to the matrix-list which was created in "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
      i <- x$getinv() ##...can be NULL or the cached Inverse
      
      ##check if the inverse "i" is already cached; if true: return it
      if(!is.null(i)) {
        message("getting cached data")
        return(i) ##this function ends here if cached Inverse was found
      }
      
      ##if no inverse was found in the cache, the inverse is calculated now:
      data <- x$get() ##...is the original input-matrix
        i <- solve(data, ...) ##inverse calculation
      x$setinv(i) ##the calculated inverse is cached (and need not be calculated again)
      i ##print the inverse
}
