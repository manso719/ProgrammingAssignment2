## These 2 functions are used to cache the inverse of an matrix value to it can 
## be repeatedly used in a much faster manner

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL              ## Clears the cache from prvious inverse value
  set <- function(y) {
    x <<- y              ## Assigns a value inside the function enclosure
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse of the same marix -returned by 
## makeCacheMatrix - was calculated earlier so it returns the cached value
## Otherwise, it computes its inverse

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse() # Checks if a cached value exists
  
  if(!is.null(i)) { 
    message("getting cached data")
    return(i)
  }
  
  # In case no cached value, the inverse should be calculated
  matrix <- x$get()  
  i <- solve(matrix) 
  x$setinverse(i)  
  i                
}

