## > source("cachematrix.R")    load R program
## > x <- makeCacheMatrix()     create a function
## > x$set(matrix(7:10, 2, 2))  set a square matrix
## > x$get()                    displaying the assigned matrix
##         [,1] [,2]
##    [1,]    7   9
##    [2,]    8  10
##
##
## > cacheSolve(x)            1st call to callSolve
##                            returning the inverse of the matrix
##        [,1] [,2]
##  [1,]   -5  4.5
##  [2,]    4 -3.5
##
## > x$getinv()
##      [,1] [,2]
##  [1,]   -5  4.5
##  [2,]    4 -3.5
##
## > cacheSolve(x)                  2nd call to cacheSolve is retrieved from
##  getting cached data             cache
##        [,1] [,2]
##  [1,]   -5  4.5
##  [2,]    4 -3.5





makeCacheMatrix <- function(x = matrix()) {
    ## input : a square invertible matrix
    ## return: a list containing functions to
    ##              1. set the matrix
    ##              2. get the matrix
    ##              3. set the inverse
    ##              4. get the inverse
    ##         this list is used as the input to cacheSolve()
    
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
    ## input : output of makeCacheMatrix()
    ## return: inverse of the original matrix input to makeCacheMatrix()
    
    inv = x$getinv()
    
    # if the inverse has already been calculated
    if (!is.null(inv)){
        # get it from the cache and skips the computation.
        message("getting cached data")
        return(inv)
    }
    
    # The inverse is not yet calculated, so we calculate it
    data = x$get()
    inv = solve(data)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(inv)
    
    return(inv)
}
