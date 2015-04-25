## Author : doug3230
## Version: April 25, 2015
## -------------------------------------------------------------------------------------------------------
## The following functions are used for creating and computing the inverse of a special kind of matrix
## (known here as a cache matrix). Cache matrices are special in that they keep their inverse stored until
## their inner values are changed. This is convenient if you frequently need to request the inverse of a
## matrix as computing a matrix's inverse is a slow operation (especially for larger matrices).
## -------------------------------------------------------------------------------------------------------

## makeCacheMatrix
## -------------------------------------------------------------------------------------------------------
## creates a matrix whose inverse can be conveniently cached after computation 
## (in other words, a cache matrix).
## -------------------------------------------------------------------------------------------------------
## Parameters:
##             x - a matrix of number values (numeric, complex, etc)
## 
## Preconditions: x is square and invertible
##
## Returns:
##         a list of functions as follows:
##                get() - gets the matrix (x)
##                set(y) - sets the matrix x equal to y
##                getinverse() - gets the cached inverse of x
##                setinverse(inverse) - sets the cached inverse equal to inverse
##
##         Note that getinverse() and setinverse() should not be called directly
##         unless you know what you are doing.
##
## Postconditions: the matrix is initialized to be x, the cached inverse is initialized to be NULL
makeCacheMatrix <- function(x = matrix()) {
  #initialize cached inverse to NULL when matrix is first made
  c_inverse <- NULL
  #sets the matrix value, reinitializes cached inverse to NULL
  set <- function(y) {
    x <<- y
    c_inverse <<- NULL
  }
  #gets the matrix value
  get <- function() x
  #sets the cached inverse value
  setinverse <- function(inverse) c_inverse <<- inverse
  #gets the cached inverse value
  getinverse <- function() c_inverse
  
  #return list of functions for getting/setting values
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## -------------------------------------------------------------------------------------------------------
## Computes the inverse of a cache matrix, taking into account that it might already have been computed 
## and cached.
## -------------------------------------------------------------------------------------------------------
## Parameters:
##             x - a cache matrix made using makeCacheMatrix()
## 
## Preconditions: x$get() is square and invertible 
##
## Returns:
##         the matrix inverse of x$get()
##
## Postconditions: The inverse value is cached in x if it wasn't already
cacheSolve <- function(x) {
  #first try getting the cached inverse
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  #when cached inverse is null, compute inverse and cache it
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
