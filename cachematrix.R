## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  This function creates a special "matrix" object inv that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## attempt to get the inverse of the matrix stored in cache
  inv <- x$getinverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # create matrix since it does not exist
  data <- x$get()
  # make sure matrix is square and invertible
  # if not, handle exception cleanly
  tryCatch({
    inv <- solve(data, ...)
  },
  error = function(e) {
    message("Error:")
    message(e)
    
    return(NA)
  },
  warning = function(e) {
    message("Warning:")
    message(e)
    
    return(NA)
  },
  finally = {
    # set inverted matrix in cache
    x$setinverse(inv)
  } )
  inv
}
