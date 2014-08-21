## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this 
## The makeCacheMatrix function will do the following:
## --  Create a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
    ## inverse.matrix <- diag(nrow(x)) %/% x
    invx <- NULL
    # print(paste("x is: ", x))
    # invx <- solve(x)
    # print(paste("invers of x is:  ", invx))
    
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    cache <- function(solve) invx <<- solve
    getcache <- function() invx
    # invx <<- solve(x)
    list(set=set, get=get, cache=cache, getcache=getcache)
    # return (invx)
}

## Write a short comment describing this function
## cacheSolve() computes the inverse of the special "matrix"
##      returned by makeCacheMatrix()
## if inverse has been calculated (and the matrix has not changed),
##      then cacheSolve retrieve inverse from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## invx <- makeCacheMatrix(x)
    invx <- x$getcache()
    if (!is.null(invx)) {
        message("retrieving cached data ...")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data, ...)
    x$cache(invx)
    invx
    
}
