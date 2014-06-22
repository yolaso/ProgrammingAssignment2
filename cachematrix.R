## Implemented two functions, one for creating a matrix that has 2 getters and 2 setters access functions
## And one for calculating the inverse of the matrix, by first checking if the inverse is stored in cache,
## and if so returning it, and if not, calculate and return it

## A function that gets a matrix, stores it in an internal variable and defines 4 access functions

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## A function that gets a matrix created with the makeCacheMatrix function and returns its inverse,
## from storage or if storage is empty, uses solve function to calculate it

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
