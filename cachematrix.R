## These function are doing cache the inverse of matrix.

## makeCacheMatrix :
##     Make a list of function that implement Cache Matrix
## 
## Parameter :
##     x(default : Empty Matrix) : Matrix need to be cached
## 
## Return :
##     list with function =>
##         set(m)    : Set matrix, m is a matrix
##         get()     : Get matrix
##         setinv(i) : Set inverse matrix, i is a matrix(inversed)
##         getinv()  : Get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (m) {
        x <<- m
        inv <<- NULL
    }
    get <- function () x
    setinv <- function (i) inv <<- i
    getinv <- function () inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve :
##     Get cached inverse matrix, if no cache, then do the inverse, cache it,
##     and return.
## 
## Parameters :
##     x : Cache Matrix
##     ... : Parameters for solve
## 
## Return :
##     Inverse of Cache Matrix

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(is.null(inv)) {
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        return(inv)
    }
    else {
        message('getting cached data')
        return(inv)
    }
}
