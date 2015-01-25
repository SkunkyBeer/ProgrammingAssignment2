
#  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#    
#    A test case here would be appreciated(!!).  That description is pretty vague
#
#    

makeCacheMatrix <- function(x = matrix()) {
  #   We need to accept a x, a matrix as parameter
  #   We'll need to intercept calls to the solve function to decide if we can retrieve a value cached here.
  #   Retrieve the value or make the initial call to solve.
  
  #   --------------------------------------------------
  #   sort of have to assume we're going to use the same structure as the example
  #   i.e. Getter/Setter methods for x and x_inv.
  #   ---------------------------------------------------
  cached <- NULL              #   our cached variable for x'
  
  set <- function(y) {        #   (potentially) new value of x
    if (is.null(x) 
        || ncol(x) != ncol(y)
        || nrow(x) != nrow(y)
        || x != y )  {
                x <<- y                   #   reset x to y in the "makeCacheMatrix" environment
                cached <<- NULL           #   reset cache to null
              }
  }
  get <- function() {x}       #   just returns x
  
  set_prime <- function (x_prime) {
              if (is.null(cached) 
                  || ncol(cached) != ncol(x_prime)
                  || nrow(cached) != nrow(x_prime)
                  || x_prime != cached) {
                          cached <<- x_prime      #   force x' into cache
                          }
              }
  get_prime <- function (){cached}
  
  #   -------------------------------
  #   Return the functions in a list
  #   ------------------------------ 
  list (
      "set" = set
      , "get" = get
      , "set_prime" = set_prime
      , "get_prime" = get_prime
      )
}


# -------------------------------------------------------------------
# cacheSolve:  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#              If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#              should retrieve the inverse from the cache.
# -------------------------------------------------------------------

#  again ... a test case.  Please...
# -------------------------------------------------------------------
cacheSolve <- function(x, ...) {

  #     Grab a cache object
  y <- makeCacheMatrix()
  
  #     set the internal cache variables to x
  y$set(x)
  y$set_prime(solve(x))
  
  z <- y$get_prime()
  if (is.null(z)) {
        #   x_prime has not been set
        y$set_prime (solve(x))
        } 
  return (y$get_prime())
  
}
