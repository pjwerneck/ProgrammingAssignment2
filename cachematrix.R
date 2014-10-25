## Special matrix that can cache its inverse


## Creates a cacheable matrix
makeCacheMatrix <- function(x = matrix()) {

  # cached inverse matrix
  i <- NULL
  
  # set the matrix locally
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
   
  # retrieve the matrix
  get <- function(){
    m
  }
  
  # set the inverse
  setinverse <- function(inverse){
    i <<- inverse
  }
  
  # get the inverse
  getinverse <- function() {
    i
  }
  
  # return all methods
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
  
}


## Compute the inverse of the matrix, or retrieve it from the cache.
cacheSolve <- function(x, ...) {

  # get the cached inverse
  i = x$getinverse()
  
  # return the cached inverse if it exists
  if( !is.null(i)){
      message("getting cached data")
      return(i)
  }
  
  # get the matrix
  data <- x$get()
  
  # calculate the inverse
  i <- solve(data) %*% data
  
  # set the inverse back
  x$setinverse(i)
  
  # return the inverse
  i  
  
}
