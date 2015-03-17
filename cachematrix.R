###############################################################################
## The functions makeCacheMatrix and cacheSolve provide examples of lexical
## scoping works in R. 
###############################################################################

## makeCacheMatrix: This function 'caches' a matrix so that it is available
##                  for use after it has completed its execution run. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## cacheSolve: This function retrieves a matrix from the 'cache' and
##             calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setmatrix(m)
    m
}
