## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Inputs: x: a solvable square matrix
## Purpose: Caches an "inverse" value for matrix x and returns a list of 4 functions:
##          -Get/Set for the matrix x
##          -Get/Set for the inverse
makeCacheMatrix <- function(x = matrix()) {
  cached_inv = NULL
  cached_matr = x
  
  set = function(y){
    cached_matr <<- y
    cached_inv <<- NULL
  }
  
  get = function(){
    cached_matr
  }
  
  set_inv = function(inverse){
    cached_inv <<- inverse
  }
  
  get_inv = function(){
    cached_inv
  }
  
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## Write a short comment describing this function
## Inputs: x: Must be a list returned from makeCacheMatrix.
## Purpose: Returns the cached inverse for the matrix specified by x.
##          If no cached inverse exists, one is created, cached, and returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  the_inv = x$get_inv()
  
  if(!is.null(the_inv)){
    message("Showing the cached inverse...")
    return(the_inv)
  }
  
  #No cached inverse exists, so create one.
  the_matrix = x$get()
  message("Solving the matrix...")
  the_inv = solve(the_matrix, ...)
  x$set_inv(the_inv)
  the_inv
  
}

# Test Code
message("-------------------")
m = matrix(round(runif(n=9, min=-10, max=10), digits=0),nrow=3)
my_m = makeCacheMatrix(m)
my_m$get()
my_m$get_inv()  #should be null
cacheSolve(my_m)  #should get the inverse, not from cache
cacheSolve(my_m)  #should get the inverse, from cache
my_m$get_inv()  #should get the inverse, from cache

m2 = matrix(round(runif(n=9, min=-10, max=10), digits=0),nrow=3)
my_m$set(m2)
my_m$get()
my_m$get_inv()  #should be null
cacheSolve(my_m)  #should get the inverse, not from cache
cacheSolve(my_m)  #should get the inverse, from cache
my_m$get_inv()  #should get the inverse, from cache

