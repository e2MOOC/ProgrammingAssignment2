## Create a list to store the address of the matrix and its inverse
## Takes in a matrix x
makeCacheMatrix <- function(x = matrix()) {
  ### interal varibale i to save address of inverse matrix
  i <- NULL
  ### function to assign address of matrix y to internal variable x 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ### function to return the matrix x (which is matrix y originally assigned)
  get <- function() x
  ### function to assign address of inverse matrix to variable i
  setInverse <- function(inverse) i <<- inverse
  ### function to return the inverse matrix i (assigned previously)
  getInverse <- function() i
  ### return a list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Retrieve the stored inverse from list if exist, else use solve to get it after putting into list
cacheSolve <- function(x, ...) {
  ### retrieve the inverse matrix from the list x
  i <- x$getInverse()
  ### if inverse matrix exist in list x, return this result
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ### continue with operation if inverse matrix does not exist
  ### assign data from list x into variable data
  data <- x$get()
  ### assign the result of solve to variable i
  i <- solve(data, ...)
  ### push the address of inverse matrix i into list x
  x$setInverse(i)
  ### return the inverse matrix i
  i
}
