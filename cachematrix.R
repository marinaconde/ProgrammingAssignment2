#returns a list of functions to store into/recover from the cache the inverse of the matrix specified using the function "set"
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  #function to cache the matrix received as argument
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #function to get from cache the matrix received as argument
  get <- function() {
    x
  }
  
  #fuction to cache the inverse of the matrix received as argument
  setinverse <- function(inverse) {
    m <<- inverse
  }
  
  #function to get from cache the inverse of the matrix received as argument
  getinverse <- function() {
    m
  }
  
  #return list of functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Return the inverse of the matrix "x" from the cache. The first call to this function, will store the inverse in the cache in order to avoid further calls to compute it again
cacheSolve <- function(x, ...) {
  
  #get the inverse of the "input matrix associated with the list of functions x"
  m <- x$getinverse()
  
  #if the inverse has not been computed yet, compute it and cache it
  if (is.null(m)) {
    
    #get the matrix whose inverse can be cached with the function "setinverse" of the list x
    data <- x$get()
    
    #compute and cache its inverse 
    #to cache it, use one of the functions in the list x
    x$setinverse(solve(data));
    m <- x$getinverse();
  } 
  #otherwise, we already got it from the cache. Thus, we just need to return it
  else {
    message("getting cached inverse")
  }  
  
  #return the cached inverse
  m    
}

