library(dplyr)
library(MASS)
### A function that can set the value of a matrix
makeCacheMatrix1 <- function(x = matrix()){
  ### the input is a matrix
  ### the inverse matrix is set to null
  inverse_mat1 <- NULL
  ### x and inverse_mat1 are global variables
  ### the function below sets the global variable received input is set
  set <- function(y){
    ### x is set to the matrix passed in via y-argument
    ### The global variable is assigned a matrix through the local variable
    x <<- y
    inverse_mat1 <<- NULL
    ### The last line is returned by the set function.
    ### This last line returns a null matrix
  }
  ### get function gets the matrix
  ### returns the global variable x that denotes the matrix
  get <- function() x
  ### 
  set_inverse1 <- function(inverse) inverse_mat1 <<- inverse
  ### inverse_mat1 is a global variable
  ### function inverse is passed on to the global variable in this set_inverse1 function
  ### The set_inverse1 function also returns the inverse_mat1 global variable
  ### As inverse_mat1 is a global variable, the output returned by the function does not matter
  
  ### get_inverse1 function returns the inverse of a matrix
  get_inverse1 <- function() {
    inver <- ginv(x)
    inver%*%x
  }
  ### returns a list of value name pair
  ### Value is the variable holding the function
  ### name refers to the function created above
  list(set = set, get = get, set_inverse1 = set_inverse1, get_inverse1 = get_inverse1)
}

### The below function sees whether the inverse of the input matrix is already available
### If the inverse is already available, it is retrieved from cache instead of repeating the calculation
### If the inverse is not calculated, the function calculates the inverse and stores the inverse in cache
cacheSolve1 <- function(x, ...) {
  ### ... refers to the optional arguments that may passed into the function
  ### original matrix is fed as an input to makeCacheMatrix
  ### Irrespective of whether the matrix is calculated or already stored,
  ### inverse of the original matrix will be returned
  inverse_mat1 <- x$get_inverse1()
  ### If the inverse of the matrix is already calculated
  ### get it from the cache
  if(!is.null(inverse_mat1)) {
    message("getting cached data")
    return(inverse_mat1)
    ### skip the computation necessary for computing 
    ### inverse as it is already cached
  }
  ### If inverse is not available already, 
  ### extract the matrix and store it in data for calculating the inverse
  data <- x$get()
  ### The  data is passed on to the function to calculate the inverse
  inverse_mat1 <- solve(data,...)
  ### The value of inverse matrix is set in the cache by
  ### the following set_inverse function
  x$set_inverse(inverse_mat1)
  inverse_mat1
}
f <- makeCacheMatrix1(matrix(1:10,2,5))
f$get()
f$get()
f$get_inverse1()
cacheSolve1(f)
