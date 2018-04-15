## R Programming Assignment 2
##The funtion takes a vector and dimensions of a matrix to convert into a special matrix within it 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Now a list from the above funtion is passed through the following function which will return 
## the inverse of the special matrix got created in the makeCacheMatrix() funtion  
## The function also checks if the special matrix is singular and its inverse is NaN. In such case,  
## the function returns a typed message that the matrix is a "Singular Matrix"

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  dm <- det(data)
  if (dm==0)
  {
    message("Singural Matrix")
    return()
  }
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
