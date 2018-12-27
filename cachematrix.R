## This function is about performing 'cache' to the matrix with get() and set() function


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse #calculate the inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)

}


## This function is used to calculate the inverse of special matrix 
## created by makeCacheMatrix above
## Using cache, it can save computing power

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
