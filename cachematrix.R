## Below are two functions that are used to create a special object 
##  that stores a matrix and cache's its inverse matrix.

## This function "makeMatrix" creates a special "matrix", 
## which is really a list containing several functions. 

makeMatrix <- function(x = matrix()) {
  t <- NULL
  set <- function(y) {  # set the value of the matrix
    x <<- y
    t <<- NULL
  }
  get <- function() x  # get the value of the matrix
  setinverse <- function(inverse) t <<- inverse  # set the value of the inverse matrix
  getinverse <- function() t # get the value of the inverse matrix 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
  
}


## This function "cacheSolve" calculates the inverse matrix of the special 
## "matrix" created with the above "makeMatrix" function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  t <- x$getinverse() # check if the inverse matrix has already been calculated
  if(!is.null(t)) {   # if it has been caculated, it gets the inverse matrix from the cache
    message("getting cached data") 
    return(t)    # skip the caculation.
  }
  data <- x$get()   # get the matrix
  t <- solve(data, ...) # calculate the inverse matrix
  x$setinverse(t) # put the value of the inverse matrix into cache
  
