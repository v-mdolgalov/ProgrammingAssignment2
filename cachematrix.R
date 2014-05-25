## This is the implementation of R Programming course assignment
## for calculating and caching inverse matrix 
## (c) Mykola Dolgalov

## This function is like defining an interface - it defines an "object" that works with inverted matricies

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- solve 
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function first checks the cache, if inverse is there, it returns inverse, otherwise calculats
## and returns inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', it utilizes cache created by function makeCacheMatrix
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
