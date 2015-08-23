## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 t <- NULL
  set <- function(y) {
    x <<- y
    t <<- NULL
  }
  get <- function() x
  settranspose <- function(transpose) t <<- transpose
  gettranspose <- function() t
  list(set = set, get = get,
       settranspose = settranspose,
       gettranspose = gettranspose) 

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  t <- x$transpose()
  if(!is.null(t)) {
    message("getting cached data")
    return(t)
  }
  data <- x$get()   # get the data 
  t <- solve(data, ...) # calculate the inverse matrix
  x$settranspose(t) # put it into cashe
  t
}
