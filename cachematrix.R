##
## makeCacheMatrix() : create a matrix 'object' with setter and getter
##                     functions for matrix and cache value
## cacheSolve()      : get the inverse for a matrix created by 
##                    makeCacheMatrix(), either from cache or by
##                    calculating the results
## testCache()       : simple test function




# Build a cache object with 4 methods to get/set raw values and results
# Used by cacheSolve() function
makeCacheMatrix <- function(c_m = matrix()) {
  c_inv <- NULL
  set <- function(m) {
    c_m <<- m
    c_inv <<- NULL
  }
  get <- function() { c_m }
  setsolved <- function(inv) {c_inv <<- inv}
  getsolved <- function() { c_inv }
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}

# Returns the inverse of a matrix object created by makeCacheMatrix()
# Checks for the cached value and returns it, if present, otherwise
# calculates the value and returns it
cacheSolve <- function(cache_m, ...) {
  # Get value or NULL from cache
  inv <- cache_m$getsolved()
  if(!is.null(inv)) {
    #Cache set, return data
    message("getting cached data")
    return(inv)
  }
  #cache not set, solve and store value, then return
  message("solving matrix")
  mat <- cache_m$get()
  inv <- solve(mat, ...)
  cache_m$setsolved(inv)
  inv
}

#Testfunction for the cache 
testCache <- function() {
  m <- matrix(c(2,3,2,2),nrow=2)
  cm <- makeCacheMatrix(m)
  print(cacheSolve(cm))
  print (cacheSolve(cm))
}