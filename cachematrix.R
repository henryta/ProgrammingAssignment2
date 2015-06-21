## The following functions were created as part of Coursera R Programming course and 
## demonstrate the use of Lexical scoping to cache variables between function calls
## through the use of the <<- operator.

## The makeCacheMatrix function returns a vector with a four functions:
##  set_matrix - validates if matrix has changed and sets the value of the matrix 
##  get_matrix - returns the value of the matrix 
##  set_matrix_inv - assigns the matrix inverse value to the 'cache'
##  get_matrix_inv - retreives the matrix inverse value which was 'cached'

## Example of use of functions:
## Create a matrix eg: 
##    mymatrixB <- matrix(c(4,3,2,1),nrow=2,ncol=2)
## Create Vector of functions by passing matrix to makeCacheMatrix eg:
##    matrix_functions <- makeCacheMatrix(mymatrixB)
## Pass the list of functions to cacheSolve to calculate inverse matrix eg:
##    cacheSolve(matrix_functions)
## Other matrices can now be calculated by calling set_matrix() function eg:
##    matrix_functions$set_matrix(mymatrixA) 

#############Sample Output########################
##  > mymatrixB = matrix(c(4,3,2,1),nrow=2,ncol=2)
##  > matrix_functions <- makeCacheMatrix(mymatrixB)
##  > cacheSolve(matrix_functions)
##        [,1] [,2]
##  [1,] -0.5    1
##  [2,]  1.5   -2
##  > cacheSolve(matrix_functions)
##  retrieving inverse matrix from cached data..
##        [,1] [,2]
##  [1,] -0.5    1
##  [2,]  1.5   -2
##  > matrix_functions$set_matrix(mymatrixB)
##  matrix has not changed..
##  > cacheSolve(matrix_functions)
##  retrieving inverse matrix from cached data..
##        [,1] [,2]
##  [1,] -0.5    1
##  [2,]  1.5   -2




makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
  
        set_matrix <- function(y) {
          ## check to verify if the matrix being passed is the same as that in cache.
          ## This ensures that even if executed several times with the same matrix
          if (!(is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y))) {
                x <<- y        ## this makes input matrix available to all functions
                inv <<- NULL   ## reset inverse matrix vaiable when matrix changes
                m <- message("new matrix..")
      
          } else {
                ## do nothing
                m <- message("matrix has not changed..")
          }
    
        }
  
        get_matrix <- function() x
        set_matrix_inv <- function(matrix_inv) inv <<- matrix_inv
        get_matrix_inv <- function() inv
    
        list(set_matrix = set_matrix, get_matrix = get_matrix,
           set_matrix_inv = set_matrix_inv, get_matrix_inv = get_matrix_inv)
  
}


## The cacheSolve function calculates inverse of the input matrix and assigns value
## to cache only if input matrix has changed.Otherwise inverse matrix in cache is returned.

cacheSolve <- function(list_of_funcs, ...) {
        ## retrieve current value of inverse matrix from cache
        inv <- list_of_funcs$get_matrix_inv()
        
        ## check if the inverse matrix variable is NULL.
        if(!is.null(inv)) {
            message("retrieving inverse matrix from cached data..")
            return(inv)
        }
        
        ##
        data <- list_of_funcs$get_matrix()
        inv <- solve(data,...)
        list_of_funcs$set_matrix_inv(inv)
        inv
    
}
