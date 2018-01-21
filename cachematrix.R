## The makeCacheMatrix function will do the following:
## --   Create a special "matrix" ... the following methods
##      are available in this function:
##      1.  set:  initialize value of matrix
##      2.  get:  returns the value of the matrix
##      3.  cache:  call solve() to get inverse matrix and stores it
##      4.  getcache:   get inverse matrix from cache

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    cache <- function(solve) invx <<- solve
    getcache <- function() invx
    
    list(set=set, get=get, cache=cache, getcache=getcache)
}

## Write a short comment describing this function
## cacheSolve() computes the inverse of the special "matrix"
##      returned by makeCacheMatrix()
## if inverse has been calculated (and the matrix has not changed),
##      then cacheSolve retrieve inverse from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    invx <- x$getcache()
    if (!is.null(invx)) {
        message("retrieving cached data ...")
        return(invx)
    }
    smatrix <- x$get()
    invx <- solve(smatrix, ...)
    x$cache(invx)
    invx
    
}
