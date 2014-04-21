## Get a matrix and make a inverse cache of then

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) 
    m <<- solve
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## If there is no Cache, return the inverse matrix

cacheSolve <- function(x, ...) {
        ## 
  m <- x$getinvert()
  if(!is.null(m)) { 
    message("getting cached data") 
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinvert(m)
  m
}
