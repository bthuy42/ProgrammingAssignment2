## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this 
## The makeCacheMatrix function will do the following:
## --  Create a special "matrix"
## --  Return cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse.matrix <- diag(nrow(x)) %/% x
    
    ## print(diag(dim(x)))
    print(paste("x is: ", x))
    print(paste("invers of x is:  ", inverse.matrix))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
