## makeCacheMatrix(x) creates a "special" matrix able to cache its inverse.
## It's implemented as a list of getter/setter functions decorating a regular 
## matrix and adding functionality of caching and retrieving its inverse
makeCacheMatrix <- function(x = matrix()) {
  
      inversed <- NULL
      
      # getters/setters
      set <- function(y) {
        x <<- y
        inversed <<- NULL
      }
      get <- function() x
      setinversed <- function(inv) inversed <<- inv
      getinversed <- function() inversed
      
      # pseudo constructor: return a list of functions
      list(set = set, get = get, setinversed = setinversed, getinversed = getinversed)
}


## cacheSolve(x) returns the inverse of the "special" matrix either by retrieving 
## it from the "special" matrix's cache or by computing it, if it hadn't been 
## previously cached, and storing in the cache for further retrievals.

cacheSolve <- function(x, ...) {
      m <- x$getinversed()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinversed(m)
      m  
}
