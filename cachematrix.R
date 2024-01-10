## Put comments here that give an overall description of what your
## functions do

## the first function gets the value of the inverse of the matrixwhen called and
## stores it in the parent environment. The function makes it possible to call 
## the value of the inverse matrix using the $ operator. 
## cacheSolve makes it possible to retrieve the inverse from cache. 

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  #initialize inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  get <- function() x 
  ##set function to get matrix
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv 
  ##function for matrix inverse
  list (set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {  
  ##checking if inv is null
    message("getting cached data")
    return(inv) 
  ##return inverse
  }
  data <- x$get()
  inv <- solve(data, ...) 
  ##calculates inverse values
  x$setInverse(inv)
  inv 
  ##returns matrix that is the inverse of x
}