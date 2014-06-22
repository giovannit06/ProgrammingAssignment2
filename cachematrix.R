## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than computing 
## it repeatedly. This pair of functions cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
## Particularly, this first function creates a special "vector", that contains
## a list of functions to:
## -set the value of the matrix
## -get the value of the matrix
## -set the value of the inverse of the matrix
## -get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                   #  initialize the inverse of the matrix
    set <- function(y) {        #  this function caches the values of x and m
        x <<- y
        m <<- NULL
    }
    get <- function() x         # this function returns the cached value of x
    setsolve <- function(solve) m <<- solve   # this function set the value of m
    getsolve <- function() m     # this function return the value of m if calculated
    list(set = set, get = get,   # return the list of 4 functions
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()   # get the cached value of the inverse of the matrix
    if(!is.null(m)) {                    # it verifie if the inverse of the matrix
        message("getting cached data")   # has been already calculated
        return(m)
    }
    data <- x$get()                      # if not already calculated 
    m <- solve(data, ...)                # the function calculate the inversion of the matrix
    x$setsolve(m)
    m                                    # and returns the value of m
    
}



