# A function to create a special "matrix" and return 
# a list of functions to:
#   * get the value of the matrix
#   * set the value of the matrix
#   * set the value of its inverse
#   * get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) xinv <<- solve # assume invertible
    getinverse <- function() xinv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# A function to calculate the inverse of the special 
# "matrix" created above with the function makeCacheMatrix.
#   * It first checks to see if the inverse 'inv' has already 
#     been calculated. 
#   * If so, it gets the inverse from the cache and skips 
#     the computation. 
#   * Otherwise, it calculates the inverse of the matrix and 
#     sets the value of the inverse in the cache via the 
#     setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinverse()
    if(!is.null(xinv)) {
        message("getting cached inverse")
        return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...) # ! assumes x is invertible
    x$setinverse(xinv)
    xinv
}
