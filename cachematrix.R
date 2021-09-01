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
	

##	           [,1]        [,2]       [,3]        [,4]
##	   [1,] -0.2367351  0.06809597  0.6705549  0.02847489
##	   [2,]  0.3499535 -0.43575028 -0.3728780 -0.13517085
##	   [3,]  0.2326984  0.09142661 -0.1218538 -0.32339186
##	   [4,] -0.2422722 -0.22009828 -0.2205389 -0.21153140

