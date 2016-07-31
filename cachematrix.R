## The two functions in this program are used to create a special object that 
## stores a matrix and caches its inverse.  


## This function creates a special "matrix" object that can cache its inverse, and
## consists of a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix inverse
# 4. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
      x <<- y
      invMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(i) invMatrix <<- i
    getInverse <- function() invMatrix
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by the 
## makeCacheMatrix function.  If the inverse has already been calculated (and the
## matrix has not changed), then the function retrieves the inverse from the
## cache.
cacheSolve <- function(x, ...) {
    invMatrix <- x$getInverse()
    if (!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setInverse(invMatrix)
    invMatrix
}
