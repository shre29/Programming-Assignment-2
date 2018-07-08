## Put comments here that give an overall description of what your
## functions do
## This function is a part of programming assignment 2 for Datascience 
## course on coursera.
## This function takes a square matrix and gives its inverse.



## Write a short comment describing this function
##makeCachematrix will take a square matrix as input.
## This function has four functions in it set_matrix(),get_matrix(),
##setInverse() and getInverse().
##set_matrix function will set the matrix provided as input to a variable 
##get_matrix function will get that matrix.
##setInverse function will set the inverse of the matrix
##getInverse function will get the the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_matrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get_matrix <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function
##cacheSolve function takes the matrix that is provided as input in
##makecacheMatrix and calculates its inverse. It stores its data in cache.
## If the new matrix is similar to the old one, it just prints out its
##inverse from the cacheSolve
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get_matrix()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  
}
myMatrix<-makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(myMatrix)
