## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Description: makeCacheMatrix creates a special "matrix" object that can
#              cache its inverse from a standard, invertible matrix x.  
# Inputs:  x - an invertible matrix
# Outputs: a list with four elements: set(), get(), setinv(), getinv()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                # initialize the inverse to NULL
    set <- function(y) {       # set() stores the input matrix  
        x <<- y
        inv <<- NULL
    }
    get <- function() x        # get() retrieves the matrix stored by set()
    setinv <- function(inverse) inv <<- inverse #setinv() stores the inverse
    getinv <- function() inv                    #getinv() retrives the inverse
    list(set = set, get = get, # return the "CacheMatrix" object
         setinv = setinv,      # the 'matrix' object is a list
         getinv = getinv)
}


## Write a short comment describing this function
# Description: cacheSolve inputs a special "CacheMatrix."
#              cacheSolve first checks if a solution is already present,
#              or cached, and returns the cached value if present. 
#              Otherwise, cacheSolve uses solve() to invert the input matrix,
#              caches the solution, and finally returns the solution to console.
# Inputs: x - a "CacheMatrix" created using makeCacheMatrix()
# Outputs: inv - the matrix inversion of x. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()      # call getinv() to retrive inv
    if(!is.null(inv)) {    # if inv is not null, retrive the cached value
        message("getting cached data")
        return(inv)
    }
    data <- x$get()        # if inv is null, call get() to retrieve input matrix 
    inv <- solve(data, ...)# use solve() to find the inverse of x
    x$setinv(inv)          # cache the computed inverse
    inv                    # print the inverse to console
}
