# example matrix inversion: assign an invertible matrix to x
# b <- makeCacheMatrix(x)
# cacheSolve(b) will output the inverse of x


# makeCacheMatrix takes in a matrix and outputs a list of functions
# used to reconstruct the matrix in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  # set() assigns a value to x and inv in the makeCacheMatrix environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get() is used to obtain the value of x in cacheSolve
  get <- function() x
  
  # set_inv() caches the value of the inverted matrix once it is computed once
  # until it is replaced
  set_inv <- function(inv_value) {
    inv <<- inv_value
  }
  
  # get_inv() provides the inverted matrix's elements
  get_inv <- function() inv
  
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
  
}

# cacheSolve uses the elements of makeCacheMatrix's output list and 
# checks if an inverse already exists in the global environment. If
# not, then it computes one then caches it.

cacheSolve <- function(x, ...) {

  # inv is assigned as the local variable that will contain the inverted matrix.
  # Checks to see if an inverse value exists and prints it if it does.
  inv <- x$get_inv()
  if(!is.null(inv)) {
    
    message("getting cached data")
    return(inv)
  }

  # evaluates then caches the inverse of the input matrix,
  # only runs if the above condition is false
  matrix <- x$get()
  inv <- solve(matrix)
  x$set_inv(inv)
  inv
}