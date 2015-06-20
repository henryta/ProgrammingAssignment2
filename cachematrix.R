## The following functions were created as part of Coursera R Programming course and 
## demonstrate the use of Lexical scoping to cache variables between function calls
## through the use of the <<- operator.

## The makeCacheMatrix function returns a vector with a four functions:
##  set_matrix - validates if matrix has changed and sets the value of the matrix 
##  get_matrix - returns the value of the matrix 
##  set_matrix_inv - assigns the matrix inverse value to the 'cache'
##  get_matrix_inv - retreives the matrix inverse value which was 'cached'


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
  
    set_matrix <- function(y) {
      if (!(is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y))) {
          x <<- y
          inv <<- NULL
          m <- message("new matrix..")
      
      } else {
          m <- message("matrix has not changed..")
      }
    
    }
  
    get_matrix <- function() x
    set_matrix_inv <- function(matrix_inv) inv <<- matrix_inv
    get_matrix_inv <- function() inv
  
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_matrix_inv = set_matrix_inv, get_matrix_inv = get_matrix_inv)
  
}


## The cacheSolve function returns the inverse of the matrix.
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_matrix_inv()
  if(!is.null(inv)) {
    message("retrieving matrix inverse from cached data..")
    return(inv)
  }
  data <- x$get_matrix()
  inv <- solve(data,...)
  x$set_matrix_inv(inv)
  inv
  
  
}
