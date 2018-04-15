## R Programming Assignment 2
##The funtion takes a vector and dimensions of a matrix to convert into a special matrix within it 

makeCacheMatrix <- function(x, nrow, ncol) {
  i <- NULL
  set <- function(y, nrow,ncol) {
    x <<- y
    i <<- NULL
  }
  get <- function() {matrix(x,nrow,ncol)}
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Now a list from the above funtion is passed through the following function which will return 
## the inverse of the special matrix got created in the makeCacheMatrix() funtion  
## The function also checks if the special matrix is singular and its inverse is NaN. In such case,  
## the function returns a typed message that the matrix is a "Singular Matrix"

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  dm <- det(data)
  if (dm==0)
  {
    message("Singural Matrix")
    return()
  }
  i <- solve(data)
  x$setinverse(i)
  i
}
