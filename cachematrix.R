## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function

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
  t
  }



