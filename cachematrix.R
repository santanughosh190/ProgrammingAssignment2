## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a function which creates a matrix that can cache its inverse for the input (given that the matrix is invertible and square matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function
## cacheSolve is a function which computes the inverse of the matrix returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return the inverse of the matrix
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv

	}
	

	## ---------------Checking the program------------------------
	## x <- matrix(rnorm(16),4,4)
	## x1 <- makeCacheMatrix(x)
	## cacheSolve(x1)
	

	## [,1]       [,2]       [,3]       [,4]
	## [1,] -0.1653269  0.2592203  0.6176218 -0.7520955
	## [2,]  0.2828334 -0.1853499  0.4511382  0.2094365
	## [3,]  0.1434840  1.0413868 -0.3550853 -0.3261154
	## [4,]  0.1793583 -0.4252171 -0.4371493 -0.1749830
	

