## Below are two functions used to create a special
## object that stores a matrix and caches its inverse.


## makeCacheMatrix creates a special matrix, which is 
## really a list containing a function to set and get
## the value of the matrix, and the value of its inverse.


makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinv <- function(solve) v <<- solve
        getinv <- function() v
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve calculates and prints the inverse of the 
## special matrix from makeCacheMatrix using the setinv
## function. If it has already been calculated, it retrieves
## the inverse from the cache and skips the computation.


cacheSolve <- function(x, ...) {
        v <- x$getinv()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinv(v)
        v
}        