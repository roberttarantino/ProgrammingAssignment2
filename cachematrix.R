##TEMPLATE

## FUNCTIONS - 
## makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse
## cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {

  cacheMatrix <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  getMatrix <- function() x
  setCache <- function(inverse) cacheMatrix <<- inverse
  getCache <- function() cacheMatrix
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
}

cacheSolve <- function(x, ...) {
  
   cacheMatrix <- x$getCache()
  
  if (!is.null(cacheMatrix)) {
    message("loading cache matrix...")
    return(cacheMatrix)
  }
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
}
}
