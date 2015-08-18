

## This function will help us set up functions like set,get,setrev and getrev which will help us in caching

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setrev <- function(rev) m <<- rev
  getrev <- function() m
  list(set = set, get = get,
       setrev = setrev,
       getrev = getrev)
}


## This function will help us calculate the reverse of the matrix with the above defined functions

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' and displa message if cached data is retrieved
  m <- x$getrev()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##to get the matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setrev(m)
  m
}
