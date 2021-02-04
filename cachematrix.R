## The goal is to have two functions, one that makes and caches an inverse
## matrix and one that checks to see if there is a cached matrix before running

## This first function makes and caches the inverse matrix. I based the 
## structure on the example provided for makeVector 

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
    x <<- y
    m <<- NULL 
  }
  get <- function() x
  setInvm<- function() m <<- solve(x)
  getInvm<- function() m
  list(set = set, get = get, setInvm = setInvm, getInvm = getInvm)
}


## This second function checks if there is a cached inverse matrix and 
## returns the cached inverse or generates it 

cacheSolve <- function(x, ...) {
  m<- x$getInvm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
    m <- solve(data,...)
    x$setInvm(m)
    m
          
}
