## The following two function are designed to enable the user
## to cmpute the inverse of a matrix and cache the result for
## future use.

## This function will compute the inverse of a matrix and cache the result.
## The function was based on the programing framework provided for the 
## makeVector function.

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv_mat <<- solve
        getinverse <- function() inv_mat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will return the inverse of a matrix if it was previously
## computed and cached. The function was built on the framework provided
## for the cachemean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$setinverse(inv_mat)
}
